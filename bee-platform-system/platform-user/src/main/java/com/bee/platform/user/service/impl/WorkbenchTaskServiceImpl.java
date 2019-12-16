package com.bee.platform.user.service.impl;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.UserInfo;
import com.bee.platform.common.utils.DateUtils;
import com.bee.platform.user.dao.mapper.WorkbenchTaskMapper;
import com.bee.platform.user.entity.WorkbenchTask;
import com.bee.platform.user.rq.WorkbenchTaskRQ;
import com.bee.platform.user.service.WorkbenchTaskService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * <p>
 * 工作台任务表 服务实现类
 * </p>
 *
 * @author hongchuan.he123
 * @since 2019-04-26
 */
@Service
public class WorkbenchTaskServiceImpl extends ServiceImpl<WorkbenchTaskMapper, WorkbenchTask> implements WorkbenchTaskService {

    @Autowired
    private WorkbenchTaskMapper workbenchTaskMapper;

    /**
     * 获取工作台任务列表
     * @param userInfo
     * @return
     */
    @Override
    public List<WorkbenchTask> listWorkbenchTask(AuthPlatformUserInfo userInfo, WorkbenchTaskRQ workbenchTaskRQ, Pagination pagination) {
        Map<String, Object> map = new HashMap<>();
        //开始时间
        if (!StringUtils.isEmpty(workbenchTaskRQ.getStartTime()) && DateUtils.isValidDate(workbenchTaskRQ.getStartTime())) {
            map.put("startTime", workbenchTaskRQ.getStartTime() + " 00:00:00");
        }
        //结束时间
        if (!StringUtils.isEmpty(workbenchTaskRQ.getStartTime()) && DateUtils.isValidDate(workbenchTaskRQ.getStartTime())) {
            map.put("endTime", workbenchTaskRQ.getEndTime() + " 23:59:59");
        }
        //任务类型
        if (!StringUtils.isEmpty(workbenchTaskRQ.getTaskType())){
            map.put("taskType", workbenchTaskRQ.getTaskType().toString());
        }
        //处理状态
        if (!StringUtils.isEmpty(workbenchTaskRQ.getTaskStatu())){
            map.put("taskStatu", workbenchTaskRQ.getTaskStatu());
        }
        return workbenchTaskMapper.listWorkbenchTask(map, pagination);
    }

}
