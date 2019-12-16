package com.bee.platform.costcontrol.dao.mapper;

import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.bee.platform.costcontrol.entity.ErpCostAllocationCr;
import com.bee.platform.user.dto.AuthUserBoxDTO;

import java.util.List;

/**
 * <p>
 * erp成本小工具-成本配置 Mapper 接口
 * </p>
 *
 * @author liliang123
 * @since 2019-06-24
 */
public interface ErpCostAllocationMapper extends BaseMapper<ErpCostAllocationCr> {

    /**
     * 查询成本配置的创建人
     * @param ids
     * @return
     */
    List<AuthUserBoxDTO> queryCreator(List<Integer> ids);

}
