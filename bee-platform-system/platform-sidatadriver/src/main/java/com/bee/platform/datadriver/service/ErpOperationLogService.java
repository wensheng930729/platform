package com.bee.platform.datadriver.service;

import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.datadriver.dto.ErpOperationLogDTO;
import com.bee.platform.datadriver.entity.ErpOperationLog;

import java.util.List;

/**
 * <p>
 * 操作日志表 服务类
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
public interface ErpOperationLogService extends IService<ErpOperationLog> {

    /**
     * 根据业务类型和企业id查询日志
     */
    ResponseResult<List<ErpOperationLogDTO>> getOperationLog(String businessType, Page page, Integer orgId, Integer orderId);
    void saveLog(Integer companyId, AuthPlatformUserInfo userInfo, Integer businessId, String businessType, String msg);
}
