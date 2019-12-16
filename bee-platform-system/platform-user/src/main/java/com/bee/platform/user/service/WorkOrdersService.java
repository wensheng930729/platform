package com.bee.platform.user.service;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.user.dto.WorkOrdersDTO;
import com.bee.platform.user.entity.WorkOrders;
import com.bee.platform.user.rq.WorkOrdersInfoRQ;
import com.bee.platform.user.rq.WorkOrdersReplyRQ;

import java.util.List;

/**
 * <p>
 * 工单信息表 服务类
 * </p>
 *
 * @author qhwang
 * @since 2019-04-25
 */
public interface WorkOrdersService extends IService<WorkOrders> {

    /**
     * 根据条件查询工单信息列表
     * @param workOrdersNumber
     * @param startTime
     * @param endTime
     * @param priority
     * @param orderStatus
     * @param page
     * @param userOrder
     * @return
     */
    List<WorkOrdersDTO> getWorkOrdersList(String workOrdersNumber, String startTime, String endTime, String priority, String orderStatus, AuthPlatformUserInfo userInfo, Pagination page, Boolean userOrder);

    /**
     * 根据工单编号查询工单详情
     * @param workOrdersNumber
     * @return
     */
    ResponseResult<WorkOrdersDTO> getWorkOrdersDetail(String workOrdersNumber);

    /**
     * 工单沟通回复(用户询问)
     * @param workOrdersReplyRQ
     * @param userInfo
     * @return
     */
    ResponseResult replyWorkOrders(WorkOrdersReplyRQ workOrdersReplyRQ, AuthPlatformUserInfo userInfo);

    /**
     * 工单沟通回复(工作人员答复)
     * @param workOrdersReplyRQ
     * @param userInfo
     * @return
     */
    ResponseResult replyAnswerWorkOrders(WorkOrdersReplyRQ workOrdersReplyRQ, AuthPlatformUserInfo userInfo);

    /**
     * 新建工单
     * @param workOrdersInfoRQ
     * @param userInfo
     * @return
     */
    ResponseResult createWorkOrders(WorkOrdersInfoRQ workOrdersInfoRQ, AuthPlatformUserInfo userInfo);


    /**
     * 确认工单已经解决
     * @param workOrdersReplyRQ
     * @param userInfo
     * @return
     */
    ResponseResult doneWorkOrders(WorkOrdersReplyRQ workOrdersReplyRQ, AuthPlatformUserInfo userInfo);

    /**
     * 查询待办的任务信息
     * @param pageSize
     * @param companyId
     * @return
     */
    ResponseResult getNeedToBeDealtInfo(Integer pageSize, Integer companyId);

}
