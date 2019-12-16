package com.bee.platform.user.controller;


import com.bee.platform.common.entity.*;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.WebUtils;
import com.bee.platform.user.authority.service.AuthPlatformUserService;
import com.bee.platform.user.dto.FeedbackDTO;
import com.bee.platform.user.dto.FeedbackDetailDTO;
import com.bee.platform.user.dto.FeedbackRecordDTO;
import com.bee.platform.user.rq.FeedbackRQ;
import com.bee.platform.user.service.FeedbackService;
import com.bee.platform.user.service.ManageUserService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import java.util.List;

/**
 * <p>
 * 意见反馈 前端控制器
 * </p>
 *
 * @author liliang123
 * @since 2019-04-28
 */
@Slf4j
@RestController
@RequestMapping("/feedback")
@Api(value = "feedback", tags = "意见反馈相关接口")
public class FeedbackController {

    @Autowired
    private FeedbackService feedbackService;
    @Autowired
    private AuthPlatformUserService usersService;
    @Autowired
    private ManageUserService manageUserService;

    @ApiOperation(value = "查询意见反馈列表")
    @RequestMapping(value = "/all", method = RequestMethod.POST)
    ResponseResult<List<FeedbackDTO>> getAll(@RequestBody Page page) {
        return feedbackService.getAll(page);
    }

    @ApiOperation(value = "查询意见反馈记录列表")
    @RequestMapping(value = "/record", method = RequestMethod.POST)
    ResponseResult<List<FeedbackRecordDTO>> getRecord(@RequestBody Page page) {
        return feedbackService.getRecord(page);
    }

    @ApiOperation(value = "根据id获取意见详情")
    @RequestMapping(value = "/get/{id}", method = RequestMethod.GET)
    ResponseResult<FeedbackDetailDTO> getById(@PathVariable Integer id) {
        if (ObjectUtils.isEmpty(id)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return feedbackService.getDetailById(id);
    }

    @ApiOperation(value = "增加意见反馈")
    @RequestMapping(value = "/add", method = RequestMethod.POST)
    ResponseResult<ResCodeEnum> add(HttpServletRequest request, @RequestBody @Validated FeedbackRQ rq) {
        AuthPlatformUserInfo userInfo = usersService.getSelfInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN, request));
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        if (ObjectUtils.isEmpty(rq)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return feedbackService.add(rq, userInfo);
    }

    @ApiOperation(value = "根据id逻辑删除意见反馈")
    @RequestMapping(value = "/delete/{id}", method = RequestMethod.GET)
    ResponseResult<ResCodeEnum> deleteById(@RequestHeader(value = "sysToken") String sysToken, @PathVariable Integer id) {
        ManagerInfo managerInfo = manageUserService.getManagerInfo(sysToken);
        if (ObjectUtils.isEmpty(managerInfo)) {
            log.error("获取管理员信息失败，类:{0} 方法:{1}", "UserController", "getUserListDetail");
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        if (ObjectUtils.isEmpty(id)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return feedbackService.deleteById(id, managerInfo);
    }


}

