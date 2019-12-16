package com.bee.platform.user.controller;


import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.common.entity.*;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.common.utils.UserInfoUtils;
import com.bee.platform.user.entity.WorkbenchTask;
import com.bee.platform.user.rq.WorkbenchTaskRQ;
import com.bee.platform.user.service.UsersService;
import com.bee.platform.user.service.WorkbenchTaskService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import java.util.List;

import static com.bee.platform.common.utils.ConstantsUtil.SYS_TOKEN;

/**
 * <p>
 * 工作台任务表 前端控制器
 * </p>
 *
 * @author jie.chen123
 * @since 2019-04-26
 */
@Slf4j
@Api(value = "workbenchTask", tags = "工作台任务相关接口")
@RestController
@CrossOrigin(origins = "*")
@RequestMapping("/workbenchTask")
public class WorkbenchTaskController {

    @Autowired
    private WorkbenchTaskService workbenchTaskService;
    @Autowired
    private UsersService usersService;

    @Autowired
    private UserInfoUtils userInfoUtils;

    @ApiOperation(value = "获取工作台任务列表", notes = "获取工作台任务列表")
    @RequestMapping(value = "/listWorkbenchTask", method = RequestMethod.POST)
    public ResponseResult<List<WorkbenchTask>> listWorkbenchTask(HttpServletRequest request, @RequestBody() WorkbenchTaskRQ workbenchTaskRQ, Page page){
        String sysToken=request.getHeader(SYS_TOKEN);
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if(ObjectUtils.isEmpty(userInfo)){
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        if(ObjectUtils.isEmpty(workbenchTaskRQ)){
            return ResponseResult.buildResponseResult(ResCodeEnum.PARAMETER_INCOMPLETE);
        }
        Pagination pagination= PageUtils.transFromPage(page);
        List<WorkbenchTask> workbenchTasks = workbenchTaskService.listWorkbenchTask(userInfo,workbenchTaskRQ,pagination);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,workbenchTasks,PageUtils.transToPage(pagination));
    }

}

