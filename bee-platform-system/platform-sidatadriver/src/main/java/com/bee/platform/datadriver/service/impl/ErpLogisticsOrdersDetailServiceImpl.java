package com.bee.platform.datadriver.service.impl;

import com.bee.platform.datadriver.entity.ErpLogisticsOrdersDetail;
import com.bee.platform.datadriver.rq.ErpLogisticsOrdersAddRQ;
import com.bee.platform.datadriver.rq.ErpLogisticsOrdersDetailAddRQ;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.datadriver.dao.mapper.ErpLogisticsOrdersDetailMapper;
import com.bee.platform.datadriver.service.ErpLogisticsOrdersDetailService;

import lombok.extern.slf4j.Slf4j;

import com.baomidou.mybatisplus.service.impl.ServiceImpl;

import java.util.Date;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * <p>
 * 物流订单明细 服务实现类
 * </p>
 *
 * @author hongchuan.he123
 * @since 2019-07-24
 */
@Slf4j
@Service
public class ErpLogisticsOrdersDetailServiceImpl extends ServiceImpl<ErpLogisticsOrdersDetailMapper, ErpLogisticsOrdersDetail> implements ErpLogisticsOrdersDetailService {
	
	@Autowired
	private ErpLogisticsOrdersDetailMapper erpLogisticsOrdersDetailMapper;
	
	  /**
     *  添加物流订单明细
     */
	@Override
	@Transactional(rollbackFor = Exception.class)
    public ResponseResult add(AuthPlatformUserInfo userInfo,ErpLogisticsOrdersDetailAddRQ rq){
		return null;
		
	}
}
