package com.bee.platform.user.authority.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.service.JedisService;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.user.authority.dao.mapper.AuthWhitelistMapper;
import com.bee.platform.user.authority.entity.AuthWhitelist;
import com.bee.platform.user.authority.service.AuthWhitelistService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * <p>
 *  服务实现类
 * </p>
 *
 * @author Raphael.dq123
 * @since 2019-05-28
 */
@Service
public class AuthWhitelistServiceImpl extends ServiceImpl<AuthWhitelistMapper, AuthWhitelist> implements AuthWhitelistService {

    @Autowired
    JedisService jedisService;
    
    @Override
    public List<AuthWhitelist> queryByPlatform() {
        String redisKey = ConstantsUtil.COMMON_AUTH_WHITELIST_KEY + "platform";
        List<AuthWhitelist> dataList = null;
         //jedisService.delKey(redisKey);
        if (jedisService.exists(redisKey)) {
             dataList = jedisService.getJsonArrayObject(redisKey, AuthWhitelist.class);
             if(null == dataList || dataList.size() < 1) {
            	 dataList = selectList(new EntityWrapper<AuthWhitelist>());
                 // 失效时间设置成两小时
                 jedisService.setJsonObject(redisKey, dataList, 2 * 60 * 60);
             }
         } else {
             dataList = selectList(new EntityWrapper<AuthWhitelist>());
             // 失效时间设置成两小时
             jedisService.setJsonObject(redisKey, dataList, 2 * 60 * 60);
         }
         return dataList;
    }

}
