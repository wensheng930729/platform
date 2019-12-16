package com.bee.platform.datadriver.service.impl;

import com.bee.platform.datadriver.entity.ErpLogisticsSettlement;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.datadriver.dao.mapper.ErpLogisticsSettlementMapper;
import com.bee.platform.datadriver.service.ErpLogisticsSettlementService;

import lombok.extern.slf4j.Slf4j;

import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * <p>
 * 物流结算 服务实现类
 * </p>
 *
 * @author hongchuan.he123
 * @since 2019-07-24
 */
@Slf4j
@Service
public class ErpLogisticsSettlementServiceImpl extends ServiceImpl<ErpLogisticsSettlementMapper, ErpLogisticsSettlement> implements ErpLogisticsSettlementService {

}
