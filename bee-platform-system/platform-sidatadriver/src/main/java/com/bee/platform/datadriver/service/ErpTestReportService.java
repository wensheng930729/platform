package com.bee.platform.datadriver.service;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.datadriver.dto.ErpTestReportDTO;
import com.bee.platform.datadriver.entity.ErpTestReport;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.datadriver.rq.ErpGetOneTestReportRQ;
import com.bee.platform.datadriver.rq.TestReportDetailRQ;
import com.bee.platform.datadriver.rq.TestReportQueryRQ;

import java.util.List;

/**
 * <p>
 * 化验单 服务类
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
public interface ErpTestReportService extends IService<ErpTestReport> {

    /**
     * 分页查询化验单列表
     * @param pagination
     * @param rq
     * @return
     */
    List<ErpTestReportDTO> listTestReport(Pagination pagination, TestReportQueryRQ rq, AuthPlatformUserInfo userInfo);

    /**
     * 删除化验单
     * @param id
     * @return
     */
    ResponseResult<Integer> deleteTestReport(AuthPlatformUserInfo userInfo, String id);

    /**
     * 查询化验单详细信息
     * @param id
     * @return
     */
    ResponseResult<ErpTestReportDTO> getTestReport(String id);

    /**
     * 保存化验单
     * @param userInfo
     * @param rq
     * @return
     */
    ResponseResult<Integer> saveTestReport(AuthPlatformUserInfo userInfo, TestReportDetailRQ rq);
}
