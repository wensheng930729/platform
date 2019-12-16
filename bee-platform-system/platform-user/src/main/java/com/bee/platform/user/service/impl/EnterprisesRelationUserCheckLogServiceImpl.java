package com.bee.platform.user.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.constants.enums.EnumCommon;
import com.bee.platform.common.entity.*;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.user.dao.mapper.EnterprisesRelationUserCheckLogMapper;
import com.bee.platform.user.dto.EnterprisesRelationUserCheckLogDTO;
import com.bee.platform.user.entity.EnterprisesRelationUserCheckLog;
import com.bee.platform.user.service.EnterprisesRelationUserCheckLogService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * <p>
 * 企业关联用户审核日志表 服务实现类
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-05
 */

@Slf4j
@Service
public class EnterprisesRelationUserCheckLogServiceImpl extends ServiceImpl<EnterprisesRelationUserCheckLogMapper, EnterprisesRelationUserCheckLog> implements EnterprisesRelationUserCheckLogService {

    /**
     * 查询用户关联企业日志信息
     * @param userInfo 用户信息
     * @param page 分页对象
     * @return 日志信息列表
     */
    @Override
    public ResponseResult<List<EnterprisesRelationUserCheckLogDTO>> getCheckLogs(AuthPlatformUserInfo userInfo, Page page) {
        Pagination pagination = PageUtils.transFromPage(page);
        log.info("查询用户关联企业日志列表");
        List<EnterprisesRelationUserCheckLog> checkLogList = baseMapper.selectPage(pagination, new EntityWrapper<EnterprisesRelationUserCheckLog>()
                .eq("create_id", userInfo.getId())
                .eq("status", EnumCommon.LogicStatus.NORMAL.getKey())
                .orderBy("operate_time",false));
        List<EnterprisesRelationUserCheckLogDTO> dtoList = BeanUtils.assemble(EnterprisesRelationUserCheckLogDTO.class, checkLogList);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,dtoList,PageUtils.transToPage(pagination));

    }
}
