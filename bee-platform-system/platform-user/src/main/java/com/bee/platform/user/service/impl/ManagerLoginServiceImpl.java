package com.bee.platform.user.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.mapper.Wrapper;
import com.bee.bcrypt.BCryptPassword;
import com.bee.platform.common.entity.*;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.service.ConfigService;
import com.bee.platform.common.service.JedisService;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.DateUtil;
import com.bee.platform.user.entity.PlatformManagers;
import com.bee.platform.user.entity.UserToken;
import com.bee.platform.user.geetest.check.VerifyLogin;
import com.bee.platform.user.service.ManagerLoginService;
import com.bee.platform.user.service.ManagersRolesService;
import com.bee.platform.user.service.PlatformManagersService;
import com.bee.platform.user.service.UserTokensService;
import com.bee.platform.user.vo.ManagerLoginVO;
import lombok.extern.slf4j.Slf4j;
import org.joda.time.DateTime;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.StringUtils;

import java.util.Date;

/**
 * @description:
 * @author: junyang.li
 * @create: 2019-04-30 10:29
 **/
@Slf4j
@Service
public class ManagerLoginServiceImpl implements ManagerLoginService {

    @Autowired
    private ConfigService configService;

    @Autowired
    private VerifyLogin verifyLogin;

    @Autowired
    private ManagersRolesService managersRolesService;

    @Autowired
    private JedisService jedisService;

    @Autowired
    private UserTokensService userTokensService;

    @Autowired
    private PlatformManagersService platformManagersService;
    /**
    * @notes 用户登录
    * @Author junyang.li
    * @Date 10:31 2019/4/30
    **/
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<String> login(ManagerLoginVO vo) {
        //极验校验
       /* boolean result=verifyLogin.verify(vo);
        if(!result){
            return ResponseResult.buildResponseResult(ResCodeEnum.UNVERIFIED);
        }*/
        Wrapper<PlatformManagers> wrapper=new EntityWrapper<PlatformManagers>().
                where("username={0}",vo.getUsername())
                .or("email={0}",vo.getUsername());
        PlatformManagers platformManagers = platformManagersService.selectOne(wrapper);
        //未查询到账号
        if(platformManagers==null|| StringUtils.isEmpty(platformManagers.getUsername())){
            log.info("未查询到管理员账号:{}",vo.getUsername());
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_PLATFORM_MANAGE);
        }
        //账号被禁用
        if(Status.FALSE.getKey().equals(platformManagers.getStatus())){
            log.info("该账号已经被禁用:{}",vo.getUsername());
            return ResponseResult.buildResponseResult(ResCodeEnum.PROHIBIT_ACCOUNT);
        }
        //密码错误
        boolean check= BCryptPassword.matches(vo.getPassword(),platformManagers.getPassword());
        if(!check){
            log.info("用户密码错误。账号是{}",vo.getUsername());
            return ResponseResult.buildResponseResult(ResCodeEnum.USERNAME_OR_PASSWORD_ERROR);
        }
        ManagerInfo managerInfo= BeanUtils.copyProperties(platformManagers, ManagerInfo.class);
        int val=Integer.valueOf(configService.getConfValue(ConstantsUtil.TOKEN_EXPIRES_IN, "7200", "Token 在redis在的失效时间"));
        Date expiresIn= DateUtil.plusSeconds((val));
        PlatformManagers manager=new PlatformManagers();
        // 如果token已经过期
        boolean overdue=expiresInIsOverdue(platformManagers.getExpiresIn());
        if (overdue) {
            // 如果token过期，则重新生成一个
            String sysToken=userTokensService.createSysToken();
            managerInfo.setSysToken(sysToken);
            manager.setSysToken(sysToken);
        }
        //查出该用户角色
        MRoleInfo roleInfo=managersRolesService.getUserRoles(managerInfo.getManagerId());
        // 重新计算过期时间点
        manager.setExpiresIn(DateUtil.plusSeconds(ConstantsUtil.OVERDUE)).setUpdateAt(new Date()).setManagerId(platformManagers.getManagerId());
        managerInfo.setExpiresIn(expiresIn).setRoleInfo(roleInfo);
        platformManagersService.updateById(manager);
        // 放入缓存，并设置失效时间
        jedisService.setObject(managerInfo.getSysToken(), managerInfo,val);
        log.info("后台管理员登录成功,用户信息是:{}",  managerInfo);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,managerInfo.getSysToken());
    }

    /**
     * @notes: 用户注销登录
     * @Author: junyang.li
     * @Date: 11:39 2019/5/21
     * @param sysToken : 用户登录凭证
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.common.entity.ResCodeEnum>
     */
    @Override
    public ResponseResult<ResCodeEnum> logout(String sysToken) {
        String value=configService.getConfValue(ConstantsUtil.LOGOUT_SWITCH,ConstantsUtil.TRUE,"退出登录时清除缓存的开关，0.关闭，1打开");
        if(ConstantsUtil.FALSE.equals(value)){
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
        }
        boolean result=jedisService.exists(sysToken);
        if(!result){
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
        }
        //将对应数据库的sysToken设置失效
        platformManagersService.update(new PlatformManagers().setExpiresIn(new Date()),
                new EntityWrapper<PlatformManagers>().where("sysToken={0}",sysToken));
        jedisService.delKey(sysToken);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * @notes: 判断token是否过期
     * @Author: junyang.li
     * @Date: 17:36 2019/5/7
     * @param expiresIn : token过期时间
     * @return: boolean
     */
    private boolean expiresInIsOverdue(Date expiresIn){
        return expiresIn==null||new DateTime(expiresIn).isAfterNow();
    }

}
