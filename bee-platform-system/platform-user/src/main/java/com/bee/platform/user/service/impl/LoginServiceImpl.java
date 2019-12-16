package com.bee.platform.user.service.impl;

import com.bee.bcrypt.BCryptPassword;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.entity.SystemCode;
import com.bee.platform.common.entity.UserInfo;
import com.bee.platform.common.service.ConfigService;
import com.bee.platform.common.service.JedisService;
import com.bee.platform.common.service.SystemCodeService;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.user.entity.User;
import com.bee.platform.user.entity.UserToken;
import com.bee.platform.user.service.LoginService;
import com.bee.platform.user.service.UserTokensService;
import com.bee.platform.user.service.UsersService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

/**
 * @description: 登录
 * @author: junyang.li
 * @create: 2019-03-04 16:54
 **/
@Slf4j
@Service
public class LoginServiceImpl implements LoginService {

    @Autowired
    private ConfigService configService;

    @Autowired
    private SystemCodeService systemCodeService;

    @Autowired
    private UsersService usersService;

    @Autowired
    private UserTokensService userTokensService;

    @Autowired
    private JedisService jedisService;
    /**
     * @notes 登录
     * @Author junyang.li
     * @Date 16:56 2019/3/4
     **/
    @Override
    public ResponseResult<String> login(String username, String password) {
        //测试账号是否允许登录
        if(!testAccountLogin(username)){
            log.info("测试账号登陆开关关闭，不允许测试账号登陆username={}",username);
            return ResponseResult.buildResponseResult(ResCodeEnum.TEST_ACCOUNT_CANNOT_LOGIN,null);
        }
        User user=usersService.selectOne(new User().setUsername(username));
        //验证密码
        if(user==null|| user.getId()==null){
            log.info("登录验证失败，数据库无法查询到相应的信息，用户账号是:{}",username);
            return ResponseResult.buildResponseResult(ResCodeEnum.ACCOUNT_NOT_FOUND,null);
        }
        if(!BCryptPassword.matches(password,user.getPassword())){
            log.info("登录验证失败，账号或密码错误，登录账号是:{}",username);
            return ResponseResult.buildResponseResult(ResCodeEnum.LOGIN_FAIL,null);
        }
        try {
            return userTokensService.getSysToken(user);
        }catch (Exception e){
            log.error("用户登录系统异常，异常信息是：{}",e);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.BUSY_SYSTEM,null);
    }

    /**
     * @notes 短信验证码登录
     * @Author junyang.li
     * @Date 10:41 2019/3/14
     **/
    @Override
    public ResponseResult<String> codeLogin(String username, String code) {
        //测试账号是否允许登录
        if(!testAccountLogin(username)){
            log.info("测试账号登陆开关关闭，不允许测试账号登陆username={}",username);
            return ResponseResult.buildResponseResult(ResCodeEnum.TEST_ACCOUNT_CANNOT_LOGIN,null);
        }
        User user=usersService.selectOne(new User().setUsername(username));
        //验证密码
        if(user==null|| user.getId()==null){
            log.info("登录验证失败，数据库无法查询到相应的信息，用户账号是:{}",username);
            return ResponseResult.buildResponseResult(ResCodeEnum.ACCOUNT_NOT_FOUND,null);
        }
        //验证验证码
        String oldCode=jedisService.get(ConstantsUtil.VALIDATE_CODE+username);
        //为空则过期
        if (StringUtils.isEmpty(oldCode)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.VALIDATE_CODE_EXPIRE);
        }
        if (!code.equals(oldCode)) {
            log.info("验证不通过,短信验证码不正确。输入的code={}，缓存中的code={}",code,oldCode);
            return ResponseResult.buildResponseResult(ResCodeEnum.VALIDATE_CODE_ERROR);
        }
        //删除session中的验证码
        jedisService.delKey(ConstantsUtil.VALIDATE_CODE+username);
        try {
            return userTokensService.getSysToken(user);
        }catch (Exception e){
            log.error("用户登录系统异常，异常信息是：{}",e);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_SYSTEM,null);
    }

    /**
     * @notes  是否允许测试账号登录
     * @Author junyang.li
     * @Date 10:46 2019/3/14
     **/
    public boolean testAccountLogin(String username){
        //测试账号是否允许登陆的开关
        String val=configService.getConfValue(ConstantsUtil.TEST_ACCOUNT_LOGIN,"1","是否允许测试账号登陆的开关。0不允许，1允许");
        //测试账号不允许登陆
        if(ConstantsUtil.TRUE.equals(val)){
            return true;
        }
        //获取测试账号列表
        List<SystemCode> list=systemCodeService.getCacheSysCodeInfo(ConstantsUtil.ALL_TEST_ACCOUNT_KEY,ConstantsUtil.TEST_ACCOUNT);
        if(CollectionUtils.isEmpty(list)){
            return true;
        }
        List<String> sysCodes=list.stream().map(SystemCode::getSysCode).collect(Collectors.toList());
        //包含则返回false不允许登录
        return !sysCodes.contains(username);
    }

    /**
     * @notes 注销登录
     * @Author junyang.li
     * @Date 9:50 2019/3/5
     **/
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> logout(String sysToken) {
        //查询退出登录时是否清除缓存的开关
        String value=configService.getConfValue(ConstantsUtil.LOGOUT_SWITCH,ConstantsUtil.TRUE,"退出登录时清除缓存的开关，0.关闭，1打开");
        if(ConstantsUtil.FALSE.equals(value)){
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
        }
        UserInfo userInfo=usersService.getSelfInfo(sysToken);
        if(userInfo==null){
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
        }
        //将对应数据库的sysToken设置失效
        UserToken userToken=new UserToken(userInfo.getUsername(),new Date());
        userTokensService.updateByParam(userToken);
        jedisService.delKey(sysToken);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }
}
