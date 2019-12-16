package com.bee.platform.datadriver.dao.mapper;

import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.datadriver.dto.ErpPurchaseStatementReportDTO;
import com.bee.platform.datadriver.dto.ErpTestReportDTO;
import com.bee.platform.datadriver.entity.ErpTestReport;
import com.bee.platform.datadriver.rq.TestReportQueryRQ;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * <p>
 * 化验单 Mapper 接口
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
public interface ErpTestReportMapper extends BaseMapper<ErpTestReport> {

    /**
     * 条件查询化验单列表
     * @param map
     * @param pagination
     * @return
     */
    List<ErpTestReport> selectReportsByCondition(TestReportQueryRQ rq, Pagination pagination);

    /**
     *
     * @param id
     * @return
     */
    ErpTestReportDTO selectDetailById(String id);

    /**
     * @Description 根据采购合同id查询验收情况
     * @Param id
     * @Date 2019/6/6 10:08
     * @Author xin.huang
     * @Return
     */
    List<ErpPurchaseStatementReportDTO> findPurchaseStatementInfoByOrderId(@Param("orderId") Integer orderId,
                                                                           @Param("productBatchId") Integer productBatchId);
}
