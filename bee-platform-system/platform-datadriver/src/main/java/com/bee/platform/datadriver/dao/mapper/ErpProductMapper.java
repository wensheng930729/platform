package com.bee.platform.datadriver.dao.mapper;

import com.bee.platform.datadriver.dto.ErpProductCategoryNameDTO;
import com.bee.platform.datadriver.entity.ErpProduct;
import com.baomidou.mybatisplus.mapper.BaseMapper;

/**
 * <p>
 * 产品档案 Mapper 接口
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
public interface ErpProductMapper extends BaseMapper<ErpProduct> {

    ErpProductCategoryNameDTO getProductCategory(Integer productId);

}
