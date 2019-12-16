package com.bee.platform.customer.service;

import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.customer.entity.AuthCustomerRole;
import com.bee.platform.user.authority.rq.CustomerRelationRoleRQ;

/**
 * <p>
 * 客户关联角色的中间表 服务类
 * </p>
 *
 * @author cheng.ke
 * @since 2019-05-20
 */
public interface AuthCustomerRoleService extends IService<AuthCustomerRole> {

   /* *//**
     * 客户关联角色、功能、应用
     * @param createId 创建人id
     * @param rq 请求参数
     * @return 操作结果
     *//*
    ResponseResult changeCustomerRole(Integer createId, CustomerRelationRoleRQ rq);*/
}
