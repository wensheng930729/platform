package com.bee.platform.customer.service;

import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.customer.entity.ErpCustomerCategoryRelation;

import java.util.List;

/**
 * <p>
 * 客户分类关联表 服务类
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-31
 */
public interface ErpCustomerCategoryRelationService extends IService<ErpCustomerCategoryRelation> {

    /**
     * 批量插入客户与分类关系
     * @param customerId
     * @param categoryIds
     */
    void addBatchRelation(Integer customerId, List<Integer> categoryIds);

    /**
     * 批量删除客户与分类关系
     * @param customerIds
     */
    void deleteBatchRelation(List<Integer> customerIds);

}
