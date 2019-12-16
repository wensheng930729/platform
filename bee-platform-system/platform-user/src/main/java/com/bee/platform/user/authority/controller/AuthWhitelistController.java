package com.bee.platform.user.authority.controller;


import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.service.JedisService;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.user.authority.service.AuthWhitelistService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

/**
 * <p>
 *  前端控制器
 * </p>
 *
 * @author Raphael.dq
 * @since 2019-05-28
 */
@RestController
@RequestMapping("/authWhitelist")
public class AuthWhitelistController {
    
    @Autowired
    JedisService jedisService;
    
    @Autowired
    AuthWhitelistService authWhitelistService;
    
    @RequestMapping("/authWhitelist/refresh")
    public ResponseResult<ResCodeEnum> refresh(@RequestParam(required = false) String platform) {
        String key = ConstantsUtil.COMMON_AUTH_WHITELIST_KEY + platform;
        jedisService.delKey(key);
        jedisService.setObject(key, authWhitelistService.queryByPlatform(), 0);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

}

