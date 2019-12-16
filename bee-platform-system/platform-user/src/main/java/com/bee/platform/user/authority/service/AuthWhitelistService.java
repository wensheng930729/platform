package com.bee.platform.user.authority.service;

import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.user.authority.entity.AuthWhitelist;

import java.util.List;

/**
 * <p>
 *  服务类
 * </p>
 *
 * @author Raphael.dq123
 * @since 2019-05-28
 */
public interface AuthWhitelistService extends IService<AuthWhitelist> {
    
    /**
     * 查询某一系统的白名单列表
     * @param
     * @return
     */
    List<AuthWhitelist> queryByPlatform();

}
