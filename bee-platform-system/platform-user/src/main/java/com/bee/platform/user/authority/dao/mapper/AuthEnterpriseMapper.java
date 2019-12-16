package com.bee.platform.user.authority.dao.mapper;

import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.bee.platform.user.authority.entity.AuthEnterprise;

import java.util.List;

/**
 * <p>
 * 企业表 Mapper 接口
 * </p>
 *
 * @author liliang123
 * @since 2019-05-23
 */
public interface AuthEnterpriseMapper extends BaseMapper<AuthEnterprise> {
    /**
     * 批量插入
     * @param list
     */
    void insertAll(List<AuthEnterprise> list);
}
