package com.bee.platform.user.service;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.business.dto.CountDTO;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.entity.UserInfo;
import com.bee.platform.user.entity.WorkbenchTask;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.user.rq.WorkbenchTaskRQ;

import java.util.List;

/**
 * <p>
 * 工作台任务表 服务类
 * </p>
 *
 * @author hongchuan.he123
 * @since 2019-04-26
 */
public interface WorkbenchTaskService extends IService<WorkbenchTask> {

    /**
     * 获取工作台任务列表
     * @param userInfo
     * @return
     */
    List<WorkbenchTask> listWorkbenchTask(AuthPlatformUserInfo userInfo, WorkbenchTaskRQ workbenchTaskRQ, Pagination pagination);


}
