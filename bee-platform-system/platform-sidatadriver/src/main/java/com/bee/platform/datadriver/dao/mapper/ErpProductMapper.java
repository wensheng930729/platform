package com.bee.platform.datadriver.dao.mapper;

import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.bee.platform.datadriver.dto.ErpProductCategoryNameDTO;
import com.bee.platform.datadriver.dto.ErpProductListByCategoryDTO;
import com.bee.platform.datadriver.entity.ErpProduct;
import org.springframework.data.repository.query.Param;

import java.util.List;
import java.util.Map;

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

    /**
     * 根据产品分类查询产品列表
     *
     * @param orgId    企业id
     * @param category 产品分类
     * @return
     */
    List<ErpProductListByCategoryDTO> getProductListByCategory(@Param("orgId") Integer orgId, @Param("category") Integer category);

}
