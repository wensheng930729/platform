package com.bee.platform.datadriver.service.impl;

import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.datadriver.dao.mapper.ErpLogisticsStatusDetailMapper;
import com.bee.platform.datadriver.entity.ErpLogisticsStatusDetail;
import com.bee.platform.datadriver.service.ErpLogisticsStatusDetailService;

import lombok.extern.slf4j.Slf4j;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * <p>
 * 物流状态明细 服务实现类
 * </p>
 *
 * @author hongchuan.he123
 * @since 2019-07-25
 */
@Service
public class ErpLogisticsStatusDetailServiceImpl extends ServiceImpl<ErpLogisticsStatusDetailMapper, ErpLogisticsStatusDetail> implements ErpLogisticsStatusDetailService {
}
