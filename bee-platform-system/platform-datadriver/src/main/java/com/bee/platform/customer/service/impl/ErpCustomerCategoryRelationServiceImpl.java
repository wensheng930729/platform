package com.bee.platform.customer.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.constants.enums.EnumCommon;
import com.bee.platform.customer.dao.mapper.ErpCustomerCategoryRelationMapper;
import com.bee.platform.customer.entity.ErpCustomerCategoryRelation;
import com.bee.platform.customer.service.ErpCustomerCategoryRelationService;
import com.google.common.collect.Lists;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Date;
import java.util.List;

/**
 * <p>
 * 客户分类关联表 服务实现类
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-31
 */
@Service
public class ErpCustomerCategoryRelationServiceImpl extends ServiceImpl<ErpCustomerCategoryRelationMapper, ErpCustomerCategoryRelation> implements ErpCustomerCategoryRelationService {

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void addBatchRelation(Integer customerId, List<Integer> categoryIds) {
        List<ErpCustomerCategoryRelation> list = Lists.newArrayList();
        for (Integer categoryId : categoryIds) {
            list.add(new ErpCustomerCategoryRelation().setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())
                    .setCustomerId(customerId)
                    .setCategoryId(categoryId));
        }
        this.insertBatch(list);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void deleteBatchRelation(List<Integer> customerIds) {
        ErpCustomerCategoryRelation entity = new ErpCustomerCategoryRelation();
        entity.setDeleted(EnumCommon.IsDeleted.is_Delete.getKey())
                .setDeletedTime(new Date());
        this.update(entity, new EntityWrapper<ErpCustomerCategoryRelation>().in("customer_id", customerIds));
    }
}
