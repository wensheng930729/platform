package com.bee.platform.dinas.datadriver.service.impl;

import com.bee.platform.dinas.datadriver.entity.DinasOperationLog;
import org.springframework.stereotype.Service;

import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.dinas.datadriver.dao.mapper.DinasOperationLogMapper;
import com.bee.platform.dinas.datadriver.entity.DinasOperationLog;
import com.bee.platform.dinas.datadriver.service.DinasOperationLogService;

/**
 * <p>
 * 操作日志表 服务实现类
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
@Service
public class DinasOperationLogServiceImpl extends ServiceImpl<DinasOperationLogMapper, DinasOperationLog> implements DinasOperationLogService {

}
