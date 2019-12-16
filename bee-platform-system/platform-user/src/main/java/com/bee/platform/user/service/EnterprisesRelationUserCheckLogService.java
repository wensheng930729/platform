package com.bee.platform.user.service;

import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.entity.UserInfo;
import com.bee.platform.user.dto.EnterprisesRelationUserCheckLogDTO;
import com.bee.platform.user.entity.EnterprisesRelationUserCheckLog;
import com.baomidou.mybatisplus.service.IService;

import java.util.List;

/**
 * <p>
 * 企业关联用户审核日志表 服务类
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-05
 */
public interface EnterprisesRelationUserCheckLogService extends IService<EnterprisesRelationUserCheckLog> {

    /**
     * 查询企业关联日志列表
     * @param userInfo 用户信息
     * @param page 分页对象
     * @return 企业关联日志列表
     */
    ResponseResult<List<EnterprisesRelationUserCheckLogDTO>> getCheckLogs(AuthPlatformUserInfo userInfo, Page page);
}
