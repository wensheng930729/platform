package com.bee.platform.user.dao.mapper;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.user.entity.WorkbenchTask;
import com.baomidou.mybatisplus.mapper.BaseMapper;

import java.util.List;
import java.util.Map;

/**
 * <p>
 * 工作台任务表 Mapper 接口
 * </p>
 *
 * @author hongchuan.he123
 * @since 2019-04-26
 */
public interface WorkbenchTaskMapper extends BaseMapper<WorkbenchTask> {

    List<WorkbenchTask> listWorkbenchTask(Map<String, Object> map, Pagination pagination);
}
