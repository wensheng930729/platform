package com.bee.platform.user.authority.service.impl;

import java.util.Date;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.bee.bcrypt.BCryptPassword;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.entity.SystemCode;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.enums.UserType;
import com.bee.platform.common.service.ConfigService;
import com.bee.platform.common.service.JedisService;
import com.bee.platform.common.service.SystemCodeService;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.user.authority.entity.AuthPlatformUser;
import com.bee.platform.user.authority.rq.AuthUserRQ;
import com.bee.platform.user.authority.service.AuthPlatformUserService;
import com.bee.platform.user.authority.service.AuthUserLoginService;
import com.bee.platform.user.authority.service.AuthUserTokensService;

import lombok.extern.slf4j.Slf4j;

/**
 * @description: 登录
 * @author: junyang.li
 * @create: 2019-03-04 16:54
 **/
@Slf4j
@Service
public class AuthUserLoginServiceImpl implements AuthUserLoginService {

    @Autowired
    private ConfigService configService;

    @Autowired
    private SystemCodeService systemCodeService;

    @Autowired
    private AuthPlatformUserService userService;

    @Autowired
    private AuthUserTokensService authUserTokensService;

    @Autowired
    private JedisService jedisService;
    /**
     * @notes 登录
     * @Author junyang.li
     * @Date 16:56 2019/3/4
     **/
    @Override
    public ResponseResult<AuthPlatformUserInfo> login(AuthUserRQ authUserRQ) {
        //测试账号是否允许登录
        if(!testAccountLogin(authUserRQ.getUsername())){
            log.info("测试账号登陆开关关闭，不允许测试账号登陆username={}",authUserRQ.getUsername());
            return ResponseResult.buildResponseResult(ResCodeEnum.TEST_ACCOUNT_CANNOT_LOGIN,null);
        }
        AuthPlatformUser user = userService.selectOne(new AuthPlatformUser()
                .setUsername(authUserRQ.getUsername()).setDeleted(Status.FALSE.getKey()));
        //验证密码
        if(Objects.isNull(user)){
            log.info("登录验证失败，数据库无法查询到相应的信息，用户账号是:{}",authUserRQ.getUsername());
            return ResponseResult.buildResponseResult(ResCodeEnum.ACCOUNT_NOT_FOUND,null);
        }
        
        //账号被禁用
        if (Status.FALSE.getKey().equals(user.getStatus())) {
            return ResponseResult.buildResponseResult(ResCodeEnum.PROHIBIT_ACCOUNT);
        }
        
        //若当前登录为后台用户登录
        if (UserType.BACKGROUND_USER.getKey().equals(authUserRQ.getUserType())) {
            //账号不是后台用户账号
            if (!UserType.BACKGROUND_USER.getKey().equals(user.getUserType())
                    && !UserType.MIDDLE_BACKGROUND_USER.getKey().equals(user.getUserType())) {
                return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_PLATFORM_MANAGE);
            }
        } else if (!UserType.MIDDLE_USER.getKey().equals(user.getUserType())
                && !UserType.MIDDLE_BACKGROUND_USER.getKey().equals(user.getUserType())){
            //判断是否是中台用户
            log.info("登录验证失败，用户尚未授权，用户账号是:{}",user.getUsername());
            return ResponseResult.buildResponseResult(ResCodeEnum.USER_NOT_AUTHORIZE,null);
        }
        
        if(!BCryptPassword.matches(authUserRQ.getPassword(),user.getPassword())){
            log.info("登录验证失败，账号或密码错误，登录账号是:{}",authUserRQ.getUsername());
            return ResponseResult.buildResponseResult(ResCodeEnum.LOGIN_FAIL,null);
        }
        try {
            return authUserTokensService.getSysToken(user);
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
    public ResponseResult<AuthPlatformUserInfo> codeLogin(String username, String code) {
        //测试账号是否允许登录
        if(!testAccountLogin(username)){
            log.info("测试账号登陆开关关闭，不允许测试账号登陆username={}",username);
            return ResponseResult.buildResponseResult(ResCodeEnum.TEST_ACCOUNT_CANNOT_LOGIN,null);
        }
        AuthPlatformUser user = userService.selectOne(new AuthPlatformUser()
                .setUsername(username).setDeleted(Status.FALSE.getKey()));
        //验证密码
        if(Objects.isNull(user)){
            log.info("登录验证失败，数据库无法查询到相应的信息，用户账号是:{}",username);
            return ResponseResult.buildResponseResult(ResCodeEnum.ACCOUNT_NOT_FOUND,null);
        }
        //账号被禁用
        if (Status.FALSE.getKey().equals(user.getStatus())) {
            return ResponseResult.buildResponseResult(ResCodeEnum.PROHIBIT_ACCOUNT);
        }

        //判断是否是中台用户
        if (!UserType.MIDDLE_USER.getKey().equals(user.getUserType())
                && !UserType.MIDDLE_BACKGROUND_USER.getKey().equals(user.getUserType())) {
            log.info("登录验证失败，用户尚未授权，用户账号是:{}",username);
            return ResponseResult.buildResponseResult(ResCodeEnum.USER_NOT_AUTHORIZE,null);

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
            return authUserTokensService.getSysToken(user);
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
        //测试账号允许登陆
        if(ConstantsUtil.TRUE.equals(val)){
            return true;
        }
        //获取测试账号列表
        List<SystemCode> list=systemCodeService.getCacheSysCodeInfo(ConstantsUtil.ALL_TEST_ACCOUNT_KEY, ConstantsUtil.TEST_ACCOUNT);
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
        String value=configService.getConfValue(ConstantsUtil.LOGOUT_SWITCH, ConstantsUtil.TRUE,"退出登录时清除缓存的开关，0.关闭，1打开");
        if(ConstantsUtil.FALSE.equals(value)){
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
        }
        AuthPlatformUserInfo userInfo= userService.getSelfInfo(sysToken);
        if(userInfo==null){
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
        }
        //将对应数据库的sysToken设置失效
        Integer loginCount = jedisService.getJsonObject(ConstantsUtil.LOGIN_COUNT_KEY + sysToken, Integer.class);
        if (Objects.isNull(loginCount) || loginCount.equals(1)) {
            jedisService.delKey(ConstantsUtil.LOGIN_COUNT_KEY + sysToken);
            userService.update(new AuthPlatformUser().setExpiresIn(new Date()),
                    new EntityWrapper<>(new AuthPlatformUser().setSysToken(sysToken)));
            jedisService.delKey(sysToken);
        } else {
            loginCount = loginCount -1;
            if (loginCount > 0) {
                int seconds = jedisService.ttl(sysToken, TimeUnit.SECONDS).intValue();
                jedisService.setJsonObject(ConstantsUtil.LOGIN_COUNT_KEY + sysToken, loginCount, seconds);
            }
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }
}
