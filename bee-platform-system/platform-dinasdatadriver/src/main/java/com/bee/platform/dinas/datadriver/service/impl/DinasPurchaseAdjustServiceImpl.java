package com.bee.platform.dinas.datadriver.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.mapper.Wrapper;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.dinas.datadriver.entity.DinasPurchaseAdjust;
import com.bee.platform.dinas.datadriver.dao.mapper.DinasPurchaseAdjustMapper;
import com.bee.platform.dinas.datadriver.entity.DinasPurchaseAdjustDetail;
import com.bee.platform.dinas.datadriver.entity.DinasPurchaseOrderDetail;
import com.bee.platform.dinas.datadriver.rq.DinasPurchaseAdjustRQ;
import com.bee.platform.dinas.datadriver.service.DinasPurchaseAdjustDetailService;
import com.bee.platform.dinas.datadriver.service.DinasPurchaseAdjustService;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.dinas.datadriver.service.DinasPurchaseOrderDetailService;
import com.bee.platform.dinas.datadriver.utils.AppendixUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import java.util.Date;
import java.util.List;

/**
 * <p>
 * 采购调价主表 服务实现类
 * </p>
 *
 * @author liliang123
 * @since 2019-08-13
 */
@Slf4j
@Service
public class DinasPurchaseAdjustServiceImpl extends ServiceImpl<DinasPurchaseAdjustMapper, DinasPurchaseAdjust> implements DinasPurchaseAdjustService {

    @Autowired
    private DinasPurchaseAdjustDetailService purchaseAdjustDetailService;
    @Autowired
    private DinasPurchaseOrderDetailService purchaseOrderDetailService;
    @Autowired
    private AppendixUtils appendixUtils;
    private static Integer ZERO = 0;

    /**
     * 添加采购调价
     * @param userInfo
     * @param rq
     * @return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<Integer> addPurchaseAdjust(AuthPlatformUserInfo userInfo, DinasPurchaseAdjustRQ rq) {
        DinasPurchaseAdjust purchaseAdjust = new DinasPurchaseAdjust();
        BeanUtils.copyProperties(rq,purchaseAdjust);
        if (!CollectionUtils.isEmpty(rq.getUrl())){
            purchaseAdjust.setUrl(appendixUtils.getJsonStr(rq.getUrl()));
        }
        purchaseAdjust.setCreateUser(userInfo.getId())
                .setCreateTime(new Date()).setDeleted(ZERO);
        // 添加调价主表信息
        if (!this.insert(purchaseAdjust)){
            log.error("新增采购订单明细失败,调用{}类{}方法出错","DinasPurchaseAdjustServiceImpl","addPurchaseAdjust()");
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_PURCHASE_ADJUST_ADD_FAILED);
        }
        if (CollectionUtils.isEmpty(rq.getAdjustDetails())){
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,purchaseAdjust.getId());
        }
        List<DinasPurchaseAdjustDetail> purchaseAdjustDetails = BeanUtils.assemble(DinasPurchaseAdjustDetail.class,rq.getAdjustDetails());
        // 修改订单明细
        for (DinasPurchaseAdjustDetail detail : purchaseAdjustDetails){
            Wrapper wrapper = new EntityWrapper<>(new DinasPurchaseOrderDetail().setDeleted(Status.FALSE.getKey()).setOrderId(rq.getOrderId())
                    .setProductId(detail.getProductId()).setProductSpecId(detail.getProductSpecId()));
            // 修改采购明细数据
            List<DinasPurchaseOrderDetail> details = purchaseOrderDetailService.selectList(wrapper);
            if (!ObjectUtils.isEmpty(details)){
                if (!purchaseOrderDetailService.update(new DinasPurchaseOrderDetail().setPrice(detail.getPriceAfter()).setTaxPrice(detail.getTaxPriceAfter()), wrapper)){
                    log.error("修改订单明细失败,调用{}类{}方法出错","DinasPurchaseAdjustServiceImpl","addPurchaseAdjust()");
                    throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_PURCHASE_DETAIL_UPDATE_FAILED);
                }
            }
            detail.setCreateUser(userInfo.getId()).setCreateTime(new Date())
                    .setAdjustId(purchaseAdjust.getId()).setDeleted(ZERO);
        }
        // 添加调价明细
        if (!purchaseAdjustDetailService.insertBatch(purchaseAdjustDetails)){
            log.error("新增采购订单明细失败,调用{}类{}方法出错","DinasPurchaseAdjustServiceImpl","addPurchaseAdjust()");
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_PURCHASE_ADJUST_DETAIL_ADD_FAILED);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,purchaseAdjust.getId());
    }
}
