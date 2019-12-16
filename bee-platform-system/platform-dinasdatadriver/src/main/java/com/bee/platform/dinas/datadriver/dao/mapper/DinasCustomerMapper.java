package com.bee.platform.dinas.datadriver.dao.mapper;

import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.dinas.datadriver.dto.DinasCustomerBoxDTO;
import com.bee.platform.dinas.datadriver.dto.DinasCustomerListDTO;
import com.bee.platform.dinas.datadriver.entity.DinasCustomer;
import com.bee.platform.dinas.datadriver.rq.DinasCustomerQueryRQ;

import java.util.List;
import java.util.Map;

/**
 * <p>
 * 客户表 Mapper 接口
 * </p>
 *
 * @author liliang123
 * @since 2019-08-13
 */
public interface DinasCustomerMapper extends BaseMapper<DinasCustomer> {

    /**
     * 客户列表
     */
    List<DinasCustomerListDTO> getCustomerList(Pagination pagination, DinasCustomerQueryRQ rq);

    /**
     * 根据类型和产品查询客户
     */
    List<DinasCustomerBoxDTO> getCustomerByTypeAndProduct(Map<String, Integer> map);
}
