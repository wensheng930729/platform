package com.bee.platform.dinas.datadriver.dao.mapper;

import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.bee.platform.dinas.datadriver.dto.DinasProductBoxDTO;
import com.bee.platform.dinas.datadriver.dto.DinasProductSpecAllocateDTO;
import com.bee.platform.dinas.datadriver.dto.DinasProductSpecBoxDTO;
import com.bee.platform.dinas.datadriver.entity.DinasProduct;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Map;

/**
 * <p>
 * 产品表 Mapper 接口
 * </p>
 *
 * @author liliang123
 * @since 2019-08-13
 */
public interface DinasProductMapper extends BaseMapper<DinasProduct> {
    /**
     * 查询所有可分配产品
     */
    List<DinasProductSpecAllocateDTO> getRelateProductAll(@Param("orgId") Integer orgId);

    /**
     * 查询客户已关联的产品-->产品-规格
     */
    List<DinasProductSpecAllocateDTO> getRelateProductCustomer(@Param("ids") List<Integer> ids);

    /**
     * 根据客户id查询客户关联产品
     */
    List<DinasProductBoxDTO> getCustomerProduct(@Param("customerId") Integer customerId);

    /**
     * 根据客户id和产品id查询客户关联产品规格
     */
    List<DinasProductSpecBoxDTO> getCustomerProductSpec(Map<String,Integer> map);
}
