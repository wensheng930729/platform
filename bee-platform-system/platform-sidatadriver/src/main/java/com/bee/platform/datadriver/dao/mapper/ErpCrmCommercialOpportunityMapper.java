package com.bee.platform.datadriver.dao.mapper;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.datadriver.dto.ErpCrmCommercialBOCDTO;
import com.bee.platform.datadriver.dto.ErpCrmSalesRankDTO;
import com.bee.platform.datadriver.entity.ErpCrmCommercialOpportunity;
import com.baomidou.mybatisplus.mapper.BaseMapper;
import org.springframework.data.repository.query.Param;

import java.util.List;
import java.util.Map;

/**
 * <p>
 * 商机信息 Mapper 接口
 * </p>
 *
 * @author chenjie123123
 * @since 2019-06-24
 */
public interface ErpCrmCommercialOpportunityMapper extends BaseMapper<ErpCrmCommercialOpportunity> {

    List<ErpCrmSalesRankDTO> getSalesRank(@Param("orgId") Integer orgId, Pagination pagination);


    List<ErpCrmSalesRankDTO> getExcelSalesRank(@Param("orgId") Integer orgId);

    List<ErpCrmCommercialOpportunity> selectByList(Map<String, Object> paramMap, Pagination pagination);
    
    List<ErpCrmCommercialBOCDTO> getexportBOC(Map<String, Object> paramMap);
}