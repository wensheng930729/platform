package com.bee.platform.user.service;

import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.entity.UserInfo;
import com.bee.platform.user.dto.ArticlesTypeDTO;
import com.bee.platform.user.entity.ArticlesType;
import com.baomidou.mybatisplus.service.IService;

import java.util.List;

/**
 * <p>
 *  服务类
 * </p>
 *
 * @author jie.zhang123
 * @since 2019-04-24
 */
public interface ArticlesTypeService extends IService<ArticlesType> {
    /**
     * 查询所有公告类型
     * @return 所有公告类型列表
     */
    List<ArticlesTypeDTO> getAllArticlesType(AuthPlatformUserInfo userInfo);

    /**
     * 验证类型名称唯一性
     * @param userInfo
     * @param name
     * @return
     */
    ResponseResult<Integer> checkTypeUnique(AuthPlatformUserInfo userInfo, String name);
}
