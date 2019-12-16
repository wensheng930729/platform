package com.bee.platform.user.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.dto.RegionDTO;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.entity.UserInfo;
import com.bee.platform.common.enums.BusinessIdType;
import com.bee.platform.common.service.ConfigService;
import com.bee.platform.common.service.JedisService;
import com.bee.platform.common.service.RegionService;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.DateUtil;
import com.bee.platform.user.dao.mapper.UserTokensMapper;
import com.bee.platform.user.entity.User;
import com.bee.platform.user.entity.UserToken;
import com.bee.platform.user.service.UserTokensService;
import com.bee.platform.user.service.UsersService;
import com.bee.platform.user.utils.CenerateIdUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang3.RandomStringUtils;
import org.joda.time.DateTime;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Date;
import java.util.UUID;

/**
 * @notes 用户登录凭证
 * @Author junyang.li
 * @Date 17:12 2019/3/4
 **/
@Slf4j
@Service
public class UserTokensServiceImpl extends ServiceImpl<UserTokensMapper, UserToken> implements UserTokensService {

    @Autowired
    private ConfigService configService;

    @Autowired
    private UserTokensMapper userTokensMapper;

    @Autowired
    private JedisService jedisService;

    @Autowired
    private RegionService regionService;

    @Autowired
    private CenerateIdUtils cenerateIdUtils;

    @Autowired
    private UsersService usersService;
    /**
     * @notes 密码校验成功，获取登录凭证
     * @Author junyang.li
     * @Date 17:13 2019/3/4
     **/
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<String> getSysToken( User user) {
        //地区信息
        RegionDTO dto=regionService.selectRegion(user.getRegionid());
        //查询配置表中token的失效时间的秒数
        UserInfo userInfo = BeanUtils.copyProperties(user, UserInfo.class).setRegion(dto);
        int expireSeconds = this.getExpireSeconds();
        //根据用户名查询用户
        UserToken userToken =userTokensMapper.selectBasc(user.getUsername());
        // 计算过期时间点
        Date expiresIn = DateUtil.plusSeconds(expireSeconds);
        if (userToken == null|| StringUtils.isBlank(userToken.getSysToken())) {
            // 从新生成并插入
            String newSysToken = this.createSysToken();
            UserToken newUserToken = new UserToken().setUsername(user.getUsername()).setSysToken(newSysToken)
                    .setExpiresIn(DateUtil.plusSeconds(ConstantsUtil.OVERDUE)).setCreator("0").setCreateTime(new Date());
            userTokensMapper.insert(newUserToken);
            userInfo.setExpiresIn(expiresIn).setSysToken(newSysToken);
            // 存入缓存中
            jedisService.setObject(userInfo.getSysToken(), userInfo,expireSeconds);
            log.info("username={},放入缓存中的数据userInfo={}", user.getUsername(), userInfo);
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,newUserToken.getSysToken());
        }
        // 如果token已经过期
        if (new DateTime(userToken.getExpiresIn()).isBeforeNow()) {
            // 如果token过期，则重新生成一个
            userToken.setSysToken(this.createSysToken());
        }
        // 重新计算过期时间点
        userToken.setExpiresIn(DateUtil.plusSeconds(ConstantsUtil.OVERDUE)).setUpdateTime(new Date());
        userInfo.setExpiresIn(expiresIn).setSysToken(userToken.getSysToken()).setExpiresTime(ConstantsUtil.ONE_DAY);
        userTokensMapper.updateById(userToken);
        // 放入缓存，并设置失效时间
        jedisService.setObject(userInfo.getSysToken(), userInfo,expireSeconds);
        log.info("username={},放入缓存中的数据userInfo={}", user.getUsername(), userInfo);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,userToken.getSysToken());
    }


    /**
     * @notes 为管理用户生成业务id
     * @Author junyang.li
     * @Date 15:37 2019/4/25
     **/
    private String createBeesrvId(){
        try {
            return cenerateIdUtils.generateOrderId(BusinessIdType.USER);
        }catch (Exception e){
            log.error("调用缓存生成管理用户的业务id连接异常，异常信息是:{}",e);
            int count=usersService.selectCount(new EntityWrapper<>());
            return cenerateIdUtils.generateOrderId(BusinessIdType.USER,count);
        }
    }

    /**
     * @notes 通过username修改UserToken中的字段
     * @Author junyang.li
     * @Date 10:02 2019/3/5
     **/
    @Override
    public void updateByParam(UserToken userToken) {
        if(userToken==null||StringUtils.isEmpty(userToken.getUsername())){
            return ;
        }
        userTokensMapper.updateByParam(userToken.setUpdateTime(new Date()));
    }

    /**
     * @notes 生成新的sysToken
     * @Author junyang.li
     * @Date 17:28 2019/3/4
     **/
    @Override
    public  String createSysToken(){
        String uuid= UUID.randomUUID().toString();
        String code=RandomStringUtils.randomAlphanumeric(6);
        return new StringBuilder(ConstantsUtil.PLATFORM).append(uuid).append(ConstantsUtil.LINE)
                .append(code).toString();
    }

    /**
     * @notes 获取sysToken过期时间秒数
     * @Author junyang.li
     * @Date 15:40 2019/3/19
     **/
    @Override
    public int getExpireSeconds() {
        String val=configService.getConfValue(ConstantsUtil.TOKEN_EXPIRES_IN, "7200", "Token 在redis在的失效时间");
        return StringUtils.isEmpty(val)?0:Integer.valueOf(val);
    }
}
