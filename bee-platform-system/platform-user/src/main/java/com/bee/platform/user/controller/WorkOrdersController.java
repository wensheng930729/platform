package com.bee.platform.user.controller;


import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.common.entity.*;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.common.utils.UserInfoUtils;
import com.bee.platform.common.utils.WebUtils;
import com.bee.platform.user.authority.service.AuthPlatformUserService;
import com.bee.platform.user.authority.service.AuthPlatformUserService;
import com.bee.platform.user.dto.WorkOrdersDTO;
import com.bee.platform.user.rq.WorkOrdersInfoRQ;
import com.bee.platform.user.rq.WorkOrdersReplyRQ;
import com.bee.platform.user.service.ManageUserService;
import com.bee.platform.user.service.UsersService;
import com.bee.platform.user.service.WorkOrdersService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import javax.validation.Valid;
import java.util.List;

/**
 * <p>
 * 工单信息表 前端控制器
 * </p>
 *
 * @author qhwang
 * @since 2019-04-25
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@RequestMapping("/workOrders")
@Api(value = "工单信息相关", tags = "工单信息相关")
public class WorkOrdersController {

    @Autowired
    private WorkOrdersService workOrdersService;

//    @Autowired
//    private UsersService usersService;

    @Autowired
    private UserInfoUtils userInfoUtils;

    @Autowired
    private ManageUserService manageUserService;
    @Autowired
    private AuthPlatformUserService userService;

    @ApiOperation(value = "根据条件分页查询工单信息列表(中台)")
    @RequestMapping(value = "/getWorkOrdersPage",method = RequestMethod.GET)
    public ResponseResult<List<WorkOrdersDTO>> getWorkOrdersPage(HttpServletRequest request, String workOrdersNumber, String startTime, String endTime, String priority, String orderStatus, Page page) {
        AuthPlatformUserInfo userInfo = userService.getSelfInfo(request.getHeader(ConstantsUtil.SYS_TOKEN));
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        Pagination pagination = PageUtils.transFromPage(page);
        List<WorkOrdersDTO> workOrdersDTOS = workOrdersService
                .getWorkOrdersList(workOrdersNumber, startTime, endTime, priority, orderStatus, userInfo, pagination, true);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, workOrdersDTOS, PageUtils.transToPage(pagination));
    }

    @ApiOperation(value = "根据条件分页查询工单信息列表(后台)")
    @RequestMapping(value = "/getAllWorkOrdersPage",method = RequestMethod.GET)
    public ResponseResult<List<WorkOrdersDTO>> getAllWorkOrdersPage(HttpServletRequest request, Page page) {
        AuthPlatformUserInfo userInfo = userService.getSelfInfo(request.getHeader(ConstantsUtil.SYS_TOKEN));
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("后台管理员未登录！");
            return ResponseResult.buildResponseResult(ResCodeEnum.USER_NOT_ADMIN);
        }
        Pagination pagination = PageUtils.transFromPage(page);
        List<WorkOrdersDTO> workOrdersDTOS = workOrdersService
                .getWorkOrdersList(null, null, null, null, null, null, pagination, false);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, workOrdersDTOS, PageUtils.transToPage(pagination));
    }

    @ApiOperation(value = "根据工单编号查询工单详细信息")
    @RequestMapping(value = "/getWorkOrdersDetail",method = RequestMethod.GET)
    public ResponseResult<WorkOrdersDTO> getWorkOrdersDetail(String workOrdersNumber) {
        if (StringUtils.isEmpty(workOrdersNumber)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.PARAMETER_INCOMPLETE);
        }
        return workOrdersService.getWorkOrdersDetail(workOrdersNumber);
    }

    @ApiOperation(value = "工单沟通回复(用户询问)")
    @RequestMapping(value = "/replyWorkOrders", method = RequestMethod.POST)
    public ResponseResult replyAskWorkOrders(HttpServletRequest request, @RequestBody @Valid WorkOrdersReplyRQ workOrdersReplyRQ) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        ResponseResult result;
        try {
            result = workOrdersService.replyWorkOrders(workOrdersReplyRQ, userInfo);
        } catch (Exception e) {
            e.printStackTrace();
            result = ResponseResult.buildResponseResult(ResCodeEnum.FAILED);
        }
        return result;
    }

    @ApiOperation(value = "工单沟通回复(工作人员答复)")
    @RequestMapping(value = "/replyAnswerWorkOrders", method = RequestMethod.POST)
    public ResponseResult replyAnswerWorkOrders(HttpServletRequest request, @RequestBody @Valid WorkOrdersReplyRQ workOrdersReplyRQ) {
        /*ManagerInfo managerInfo = manageUserService.getManagerInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN,request));*/
        AuthPlatformUserInfo userInfo = userService.getSelfInfo(request.getHeader(ConstantsUtil.SYS_TOKEN));
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        ResponseResult result;
        try {
            result = workOrdersService.replyAnswerWorkOrders(workOrdersReplyRQ, userInfo);
        } catch (Exception e) {
            e.printStackTrace();
            result = ResponseResult.buildResponseResult(ResCodeEnum.FAILED);
        }
        return result;
    }

    @ApiOperation(value = "新建工单")
    @RequestMapping(value = "/createWorkOrders", method = RequestMethod.POST)
    public ResponseResult createWorkOrders(HttpServletRequest request, @RequestBody @Valid WorkOrdersInfoRQ workOrdersInfoRQ) {
        /*UserInfo userInfo = usersService.getUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN,request));*/
        AuthPlatformUserInfo userInfo = userService.getSelfInfo(request.getHeader(ConstantsUtil.SYS_TOKEN));
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        ResponseResult result;
        try {
            result = workOrdersService.createWorkOrders(workOrdersInfoRQ, userInfo);
        } catch (Exception e) {
            //e.printStackTrace();
            log.info("错误信息：",e);
            result = ResponseResult.buildResponseResult(ResCodeEnum.FAILED);
        }
        return result;
    }

    @ApiOperation(value = "确认工单已经解决")
    @RequestMapping(value = "/doneWorkOrders", method = RequestMethod.POST)
    public ResponseResult doneWorkOrders(HttpServletRequest request, @RequestBody WorkOrdersReplyRQ workOrdersReplyRQ) {
        /*UserInfo userInfo = usersService.getUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN,request));*/
        AuthPlatformUserInfo userInfo = userService.getSelfInfo(request.getHeader(ConstantsUtil.SYS_TOKEN));
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        ResponseResult result;
        try {
            result = workOrdersService.doneWorkOrders(workOrdersReplyRQ, userInfo);
        } catch (Exception e) {
            e.printStackTrace();
            result = ResponseResult.buildResponseResult(ResCodeEnum.FAILED);
        }
        return result;
    }

    @ApiOperation(value = "获取用户待办信息")
    @RequestMapping(value = "/getNeedToBeDealtInfo",method = RequestMethod.GET)
    public ResponseResult getWorkOrdersPage(HttpServletRequest request, Integer pageSize) {
        /*UserInfo userInfo = usersService.getUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN,request));*/
        AuthPlatformUserInfo userInfo = userService.getSelfInfo(request.getHeader(ConstantsUtil.SYS_TOKEN));
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return workOrdersService.getNeedToBeDealtInfo(pageSize, userInfo.getOrgId());
    }

}

