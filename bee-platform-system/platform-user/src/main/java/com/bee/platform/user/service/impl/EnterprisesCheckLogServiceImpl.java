package com.bee.platform.user.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.constants.enums.EnumCommon;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.user.authority.entity.AuthPlatformUser;
import com.bee.platform.user.dao.mapper.EnterprisesCheckLogMapper;
import com.bee.platform.user.dao.mapper.EnterprisesCheckMapper;
import com.bee.platform.user.dto.EnterprisesCheckLogDTO;
import com.bee.platform.user.entity.EnterprisesCheck;
import com.bee.platform.user.entity.EnterprisesCheckLog;
import com.bee.platform.user.service.EnterprisesCheckLogService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Date;
import java.util.List;

/**
 * <p>
 * 企业审核日志表 服务实现类
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-04-30
 */
@Slf4j
@Service
public class EnterprisesCheckLogServiceImpl extends ServiceImpl<EnterprisesCheckLogMapper, EnterprisesCheckLog> implements EnterprisesCheckLogService {
	@Autowired
	private EnterprisesCheckMapper enterprisesCheckMapper;
	@Autowired
	private EnterprisesCheckLogMapper enterprisesCheckLogMapper;
	
	/**
     * 查询用户企业申请审核日志信息
     * @param userInfo 用户信息
     * @param page 分页条件
     * @return 审核日志信息
     */
    @Override
    public ResponseResult<List<EnterprisesCheckLogDTO>> getCheckLogs(AuthPlatformUserInfo userInfo, Page page) {
        Pagination pagination = PageUtils.transFromPage(page);
        log.info("查询企业申请日志列表");
        List<EnterprisesCheckLog> checkLogList = baseMapper.selectPage(pagination, new EntityWrapper<EnterprisesCheckLog>()
                .eq("create_id", userInfo.getId())
                .eq("status", EnumCommon.LogicStatus.NORMAL.getKey())
                .orderBy("operate_time",false));
        List<EnterprisesCheckLogDTO> dtoList = BeanUtils.assemble(EnterprisesCheckLogDTO.class, checkLogList);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,dtoList,PageUtils.transToPage(pagination));
    }

	@Override
	public Integer insertEnterprisesCheckLog(Integer enterprisesCheckId, Integer operateType, Integer executResult,
			String refuseReason, AuthPlatformUserInfo userInfo, AuthPlatformUser user) {
		EnterprisesCheckLog enterprisesCheckLog = new EnterprisesCheckLog();
		EnterprisesCheck check = enterprisesCheckMapper.selectOne(new EnterprisesCheck().setId(enterprisesCheckId));
		enterprisesCheckLog.setEnterpriseCheckId(check.getId());
		enterprisesCheckLog.setEnterpriseName(check.getName());
		enterprisesCheckLog.setOperateId(user.getId());
		enterprisesCheckLog.setOperateName(user.getNickname());
		enterprisesCheckLog.setOperateType(operateType);
		enterprisesCheckLog.setOperateResult(executResult);
		enterprisesCheckLog.setOperateTime(new Date());
		enterprisesCheckLog.setRefuseReason(refuseReason);
		enterprisesCheckLog.setCreateId(user.getId());
		enterprisesCheckLog.setCreator(user.getNickname());
		enterprisesCheckLog.setCreateTime(new Date());
		Integer count = enterprisesCheckLogMapper.insert(enterprisesCheckLog);
		if (count <= Status.FALSE.getKey()) {
			log.error("插入日志记录失败！调用{}的{}方法出错 ", "EnterprisesCheckLogServiceImpl", "insertEnterprisesCheckLog()");
			throw new BusinessException(ResCodeEnum.UPDATA_FAIL, ExceptionMessageEnum.ENTERPRISESAPPSLOG_SAVE_FAILED);
		}
		return count;
	}
}
