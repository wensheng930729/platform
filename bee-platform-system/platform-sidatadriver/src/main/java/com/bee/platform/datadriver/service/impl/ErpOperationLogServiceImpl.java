package com.bee.platform.datadriver.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.datadriver.dao.mapper.ErpOperationLogMapper;
import com.bee.platform.datadriver.dto.ErpOperationLogDTO;
import com.bee.platform.datadriver.entity.ErpOperationLog;
import com.bee.platform.datadriver.enums.EnumBusinessType;
import com.bee.platform.datadriver.service.ErpOperationLogService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Date;
import java.util.List;

/**
 * <p>
 * 操作日志表 服务实现类
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
@Slf4j
@Service
public class ErpOperationLogServiceImpl extends ServiceImpl<ErpOperationLogMapper, ErpOperationLog> implements ErpOperationLogService {

    @Autowired
    private ErpOperationLogMapper operationLogMapper;

    @Override
    public ResponseResult<List<ErpOperationLogDTO>> getOperationLog(String businessType, Page page, Integer orgId, Integer orderId) {
        Pagination pagination = PageUtils.transFromPage(page);
        ErpOperationLog operationLog = new ErpOperationLog().setBusinessType(businessType)
                .setCompanyId(orgId)
                .setBusinessId(orderId);
        List<ErpOperationLog> operationLogList = operationLogMapper.selectPage(pagination, new EntityWrapper<>(operationLog)
                .orderBy("operate_time", false));
        List<ErpOperationLogDTO> operationLogDTOList = BeanUtils.assemble(ErpOperationLogDTO.class, operationLogList);
        try {
            EnumBusinessType type = EnumBusinessType.valueOf(businessType.toUpperCase());
            for (ErpOperationLogDTO dto : operationLogDTOList) {
                dto.setBusinessType(type.getValue());
            }
        } catch (IllegalArgumentException e) {
            log.info("操作日志的EnumBusinessType不正确，错误的type为{},类：{},方法：{}", businessType, "ErpOperationLogService", "getOperationLog");
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, operationLogDTOList, PageUtils.transToPage(pagination));
    }

    @Transactional(rollbackFor = Exception.class)
    @Override
    public void saveLog(Integer companyId, AuthPlatformUserInfo userInfo, Integer businessId, String businessType, String msg) {
        ErpOperationLog operationLog = new ErpOperationLog().setCompanyId(companyId).setBusinessId(businessId).setBusinessType(businessType)
                .setOperateMsg(msg).setOperator(userInfo.getId()).setOperatorName(userInfo.getName()).setOperateTime(new Date());

        if (!insert(operationLog)) {
            log.error("添加操作日志失败");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, "添加操作日志失败！");
        }
    }


}
