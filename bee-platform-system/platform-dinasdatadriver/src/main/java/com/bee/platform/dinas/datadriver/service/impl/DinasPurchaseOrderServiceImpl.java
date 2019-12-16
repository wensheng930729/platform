package com.bee.platform.dinas.datadriver.service.impl;

import com.alibaba.fastjson.JSONArray;
import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.dinas.datadriver.dao.mapper.*;
import com.bee.platform.dinas.datadriver.dto.*;
import com.bee.platform.dinas.datadriver.entity.*;
import com.bee.platform.dinas.datadriver.rq.*;
import com.bee.platform.dinas.datadriver.service.DinasPurchaseAdjustService;
import com.bee.platform.dinas.datadriver.service.DinasPurchaseOrderDetailService;
import com.bee.platform.dinas.datadriver.service.DinasPurchaseOrderService;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.dinas.datadriver.utils.AppendixUtils;
import com.google.common.collect.Lists;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

/**
 * <p>
 * 采购合同 服务实现类
 * </p>
 *
 * @author liliang123
 * @since 2019-08-13
 */
@Slf4j
@Service
public class DinasPurchaseOrderServiceImpl extends ServiceImpl<DinasPurchaseOrderMapper, DinasPurchaseOrder> implements DinasPurchaseOrderService {

    @Autowired
    private DinasPurchaseOrderDetailService purchaseOrderDetailService;
    @Autowired
    private DinasPurchaseOrderMapper purchaseOrderMapper;
    @Autowired
    private DinasPurchaseOrderDetailMapper purchaseOrderDetailMapper;
    @Autowired
    private DinasPurchaseAdjustService purchaseAdjustService;
    @Autowired
    private DinasPurchaseAdjustDetailMapper purchaseAdjustDetailMapper;
    @Autowired
    private DinasPurchasePayMapper purchasePayMapper;
    @Autowired
    private DinasPurchaseInvoiceMapper purchaseInvoiceMapper;
    @Autowired
    private DinasPurchaseSettlementMapper purchaseSettlementMapper;
    @Autowired
    private AppendixUtils appendixUtils;
    private static Integer ZERO = 0;

    /**
     * 分页查询订单列表
     * @param pagination
     * @param rq
     * @param userInfo
     * @return
     */
    @Override
    public List<DinasPurchaseOrderListDTO> listPurchaseOrder(Pagination pagination, DinasOrderQueryRQ rq, AuthPlatformUserInfo userInfo) {
        rq.setCompanyId(userInfo.getOrgId());
        return purchaseOrderMapper.listPurchaseOrderByCondition(rq, pagination);
    }

    /**
     * 获取订单详情
     * @param id
     * @return
     */
    @Override
    public DinasPurchaseOrderInfoDTO getPurchaseOrderDetail(Integer id) {
        // 订单详细信息
        DinasPurchaseOrderInfoDTO order = purchaseOrderMapper.getPurchaseOrderDetail(id);
        if (!ObjectUtils.isEmpty(order) && StringUtils.isNotEmpty(order.getUrl())){
            order.setUrlList((List<DinasUrlDTO>)JSONArray
                    .parseArray(order.getUrl(), DinasUrlDTO.class));
            order.setUrl(null);
        }
        // 订单明细信息
        List<DinasPurchaseOrderDetailDTO> details =purchaseOrderDetailMapper.listPurchaseOrderDetail(id);
        // 订单调价信息
        List<DinasPurchaseAdjust> adjusts = purchaseAdjustService.selectList(new EntityWrapper<>(new DinasPurchaseAdjust()
                .setOrderId(id).setDeleted(Status.FALSE.getKey())));
        List<DinasPurchaseAdjustDTO> adjustDtos = BeanUtils.assemble(DinasPurchaseAdjustDTO.class,adjusts);
        // 查询条件明细
        for (DinasPurchaseAdjustDTO dto : adjustDtos){
            List<DinasPurchaseAdjustDetailDTO> detailDtos = purchaseAdjustDetailMapper.listPurchaseAdjustDetail(dto.getId());
            if (StringUtils.isNotEmpty(dto.getUrl())){
                dto.setUrlList((List<DinasUrlDTO>)JSONArray
                        .parseArray(dto.getUrl(), DinasUrlDTO.class));
                dto.setUrl(null);
            }
            dto.setAdjustDetails(detailDtos);
        }
        return order.setDetailDtos(details).setAdjustDtos(adjustDtos);
    }

    /**
     * 删除订单
     * @param userInfo
     * @param ids
     * @return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<List<Integer>> deletePurchaseOrder(AuthPlatformUserInfo userInfo, List<Integer> ids) {
        this.checkIsUsed(ids);
        // 删除订单
        if(!this.update(new DinasPurchaseOrder().setDeleted(Status.TRUE.getKey()),new EntityWrapper<DinasPurchaseOrder>()
                .in("id",ids))){
            log.error("删除采购订单失败,调用{}类{}方法出错","DinasPurchaseOrderServiceImpl","deletePurchaseOrder()");
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_PURCHASE_DELETE_FAILED);
        }
        // 删除订单明细
        if (purchaseOrderDetailService.selectCount(new EntityWrapper<DinasPurchaseOrderDetail>().eq("deleted",Status.FALSE.getKey())
                .in("order_id",ids)) > ZERO){
            if(!purchaseOrderDetailService.update(new DinasPurchaseOrderDetail().setDeleted(Status.TRUE.getKey()),new EntityWrapper<DinasPurchaseOrderDetail>()
                    .in("order_id",ids))){
                log.error("删除采购订单明细失败,调用{}类{}方法出错","DinasPurchaseOrderServiceImpl","deletePurchaseOrder()");
                throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_PURCHASE_DETAIL_DELETE_FAILED);
            }
        }
        // 删除订单调价明细
        if (purchaseAdjustService.selectCount(new EntityWrapper<DinasPurchaseAdjust>().eq("deleted",Status.FALSE.getKey())
                .in("order_id",ids)) > ZERO){
            // 删除调价记录
            if(!purchaseAdjustService.update(new DinasPurchaseAdjust().setDeleted(Status.TRUE.getKey()),new EntityWrapper<DinasPurchaseAdjust>()
                    .in("order_id",ids))){
                log.error("删除采购订单调价记录失败,调用{}类{}方法出错","DinasPurchaseOrderServiceImpl","deletePurchaseOrder()");
                throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_PURCHASE_ADJUST_DETAIL_DELETE_FAILED);
            }
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,ids);
    }

    /**
     * 校验单据是否被使用
     * @param ids
     */
    private void checkIsUsed(List<Integer> ids) {
        // 校验是否被付款单使用
        if (purchasePayMapper.selectCount(new EntityWrapper<DinasPurchasePay>()
                .eq("deleted",Status.FALSE.getKey())
                .in("order_id",ids)) > ZERO){
            log.error("删除采购订单失败,调用{}类{}方法出错","DinasPurchaseOrderServiceImpl","deletePurchaseOrder()");
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_PURCHASE_USED_BY_PAY);
        }
        // 校验是否被发票使用
        if (purchaseInvoiceMapper.selectCount(new EntityWrapper<DinasPurchaseInvoice>()
                .eq("deleted",Status.FALSE.getKey())
                .in("order_id",ids)) > ZERO){
            log.error("删除采购订单调价,调用{}类{}方法出错","DinasPurchaseOrderServiceImpl","deletePurchaseOrder()");
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_PURCHASE_USED_BY_INVOICE);
        }
        // 校验是否被结算结算单使用
        if (purchaseSettlementMapper.selectCount(new EntityWrapper<DinasPurchaseSettlement>()
                .eq("deleted",Status.FALSE.getKey())
                .in("purchase_order_id",ids)) > ZERO){
            log.error("删除采购订单失败,调用{}类{}方法出错","DinasPurchaseOrderServiceImpl","deletePurchaseOrder()");
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_PURCHASE_USED_BY_SETTLEMENT);
        }
    }

    /**
     * 新增订单
     * @param userInfo
     * @param rq
     * @return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<Integer> addPurchaseOrder(AuthPlatformUserInfo userInfo, DinasPurchaseOrderAddRQ rq) {
        // 校验订单编号唯一
        if (this.selectCount(new EntityWrapper<>(new DinasPurchaseOrder().setDeleted(Status.FALSE.getKey())
                .setCompanyId(userInfo.getOrgId()).setCode(rq.getCode()))) > ZERO){
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_CODE_REPEAT);
        }
        // 校验订单明细产品规格是否重复
        if (checkDetailsIsRepeat(rq.getDetailRqs())){
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_PURCHASE_DETAIL_PRODUCTSPEC_REPEAT);
        }
        DinasPurchaseOrder order = new DinasPurchaseOrder();
        BeanUtils.copyProperties(rq,order);
        if (!CollectionUtils.isEmpty(rq.getUrl())){
            order.setUrl(appendixUtils.getJsonStr(rq.getUrl()));
        }
        order.setCompanyId(userInfo.getOrgId()).setCompanyName(userInfo.getOrg_name())
                .setCreateUser(userInfo.getId()).setCreateTime(new Date())
                .setUpdateUser(userInfo.getId()).setUpdateTime(new Date()).setDeleted(ZERO);
        if (!this.insert(order)){
            log.error("新增采购订单失败,调用{}类{}方法出错","DinasPurchaseOrderServiceImpl","addPurchaseOrder()");
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_PURCHASE_ADD_FAILED);
        }
        // 添加产品明细
        if (!CollectionUtils.isEmpty(rq.getDetailRqs())){
            List<DinasPurchaseOrderDetail> purchaseOrderDetails = BeanUtils.assemble(DinasPurchaseOrderDetail.class,rq.getDetailRqs());
            purchaseOrderDetails.stream().map(a->{
                return a.setOrderId(order.getId()).setCreateUser(userInfo.getId()).setCreateTime(new Date())
                        .setUpdateUser(userInfo.getId()).setUpdateTime(new Date()).setDeleted(ZERO);
            }).collect(Collectors.toList());
            if (!purchaseOrderDetailService.insertBatch(purchaseOrderDetails)){
                log.error("新增采购订单明细失败,调用{}类{}方法出错","DinasPurchaseOrderServiceImpl","addPurchaseOrder()");
                throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_PURCHASE_DETAIL_ADD_FAILED);
            }
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,order.getId());
    }

    /**
     * 校验订单明细是否重复
     */
    private boolean checkDetailsIsRepeat(List<DinasPurchaseOrderDetailRQ> detailRqs) {
        if (CollectionUtils.isEmpty(detailRqs)){
            return false;
        }
        List<DinasPurchaseOrderDetail> details = Lists.newArrayList();
        for (DinasPurchaseOrderDetailRQ rq : detailRqs){
            DinasPurchaseOrderDetail detail = new DinasPurchaseOrderDetail()
                    .setProductId(rq.getProductId()).setProductSpecId(rq.getProductSpecId());
            if (details.contains(detail)){
                return true;
            }
            details.add(detail);
        }
        return false;
    }

    /**
     * 校验订单调价明细是否重复
     */
    private boolean checkAdjustDetailsIsRepeat(List<DinasPurchaseAdjustDetailRQ> detailRqs) {
        if (CollectionUtils.isEmpty(detailRqs)){
            return false;
        }
        List<DinasPurchaseAdjustDetail> details = Lists.newArrayList();
        for (DinasPurchaseAdjustDetailRQ rq : detailRqs){
            DinasPurchaseAdjustDetail detail = new DinasPurchaseAdjustDetail()
                    .setProductId(rq.getProductId()).setProductSpecId(rq.getProductSpecId());
            if (details.contains(detail)){
                return true;
            }
            details.add(detail);
        }
        return false;
    }

    /**
     * 修改订单
     * @param userInfo
     * @param rq
     * @return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<Integer> updatePurchaseOrder(AuthPlatformUserInfo userInfo, DinasPurchaseOrderUpdateRQ rq) {
        // 校验订单明细是否重复
        if (checkDetailsIsRepeat(rq.getDetailRqs())){
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_PURCHASE_DETAIL_PRODUCTSPEC_REPEAT);
        }
        DinasPurchaseOrder orderOri = this.selectById(rq.getId());
        BeanUtils.copyProperties(rq,orderOri);
        if (!CollectionUtils.isEmpty(rq.getUrl())){
            orderOri.setUrl(appendixUtils.getJsonStr(rq.getUrl()));
        }
        orderOri.setUpdateUser(userInfo.getId()).setUpdateTime(new Date()).setDeleted(ZERO);
        if (!this.updateById(orderOri)){
            log.error("修改采购订单失败,调用{}类{}方法出错","DinasPurchaseOrderServiceImpl","updatePurchaseOrder()");
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_PURCHASE_UPDATE_FAILED);
        }
        // 修改订单明细
        this.updatePurchaseOrderDetail(userInfo,rq);
        // 修改调价明细
        this.updatePurchaseOrderAdjust(userInfo,rq);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,orderOri.getId());
    }

    /**
     * 修改订单明细
     * @param rq
     */
    private void updatePurchaseOrderDetail(AuthPlatformUserInfo userInfo, DinasPurchaseOrderUpdateRQ rq) {
        // 删除订单明细
        if (purchaseOrderDetailService.selectCount(new EntityWrapper<DinasPurchaseOrderDetail>().eq("deleted",Status.FALSE.getKey())
                .eq("order_id",rq.getId())) > ZERO){
            if(!purchaseOrderDetailService.update(new DinasPurchaseOrderDetail().setDeleted(Status.TRUE.getKey()),new EntityWrapper<DinasPurchaseOrderDetail>()
                    .eq("order_id",rq.getId()))){
                log.error("删除采购订单明细失败,调用{}类{}方法出错","DinasPurchaseOrderServiceImpl","deletePurchaseOrder()");
                throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_PURCHASE_DETAIL_DELETE_FAILED);
            }
        }
        // 添加产品明细
        if (!CollectionUtils.isEmpty(rq.getDetailRqs())){
            List<DinasPurchaseOrderDetail> purchaseOrderDetails = BeanUtils.assemble(DinasPurchaseOrderDetail.class,rq.getDetailRqs());
            purchaseOrderDetails.stream().map(a->{
                return a.setOrderId(rq.getId()).setCreateUser(userInfo.getId()).setCreateTime(new Date())
                        .setUpdateUser(userInfo.getId()).setUpdateTime(new Date()).setDeleted(ZERO);
            }).collect(Collectors.toList());
            if (!purchaseOrderDetailService.insertBatch(purchaseOrderDetails)){
                log.error("新增采购订单明细失败,调用{}类{}方法出错","DinasPurchaseOrderServiceImpl","addPurchaseOrder()");
                throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_PURCHASE_DETAIL_ADD_FAILED);
            }
        }

    }

    /**
     * 修改调价明细
     * @param userInfo
     * @param rq
     */
    private void updatePurchaseOrderAdjust(AuthPlatformUserInfo userInfo, DinasPurchaseOrderUpdateRQ rq) {
        List<DinasPurchaseAdjust> adjusts = purchaseAdjustService.selectList(new EntityWrapper<DinasPurchaseAdjust>().eq("deleted",Status.FALSE.getKey())
                .eq("order_id",rq.getId()));
        // 删除订单明细
        if (!CollectionUtils.isEmpty(adjusts)){
            // 删除调价
            if(!purchaseAdjustService.update(new DinasPurchaseAdjust().setDeleted(Status.TRUE.getKey()),
                    new EntityWrapper<DinasPurchaseAdjust>().eq("order_id",rq.getId()))){
                log.error("删除采购调价失败,调用{}类{}方法出错","DinasPurchaseOrderServiceImpl","deletePurchaseOrder()");
                throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_PURCHASE_ADJUST_DELETE_FAILED);
            }
            // 删除调价明细
            if (purchaseAdjustDetailMapper.update(new DinasPurchaseAdjustDetail().setDeleted(Status.TRUE.getKey()),
                    new EntityWrapper<DinasPurchaseAdjustDetail>()
                            .in("adjust_id",adjusts.stream().map(a->a.getId()).collect(Collectors.toList()))) < ZERO){
                log.error("删除采购调价明细失败,调用{}类{}方法出错","DinasPurchaseOrderServiceImpl","deletePurchaseOrder()");
                throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_PURCHASE_ADJUST_DETAIL_DELETE_FAILED);
            }
        }
        if(!CollectionUtils.isEmpty(rq.getAdjustDtos())){
            for (DinasPurchaseAdjustRQ adjustRQ: rq.getAdjustDtos()){
                // 校验订单明细产品规格是否重复
                if (checkAdjustDetailsIsRepeat(adjustRQ.getAdjustDetails())){
                    throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_ADJUST_DETAIL_PRODUCTSPEC_REPEAT);
                }
                purchaseAdjustService.addPurchaseAdjust(userInfo,adjustRQ);
            }
        }

    }

    /**
     * 获取订单使用的产品
     * @param id
     * @return
     */
    @Override
    public ResponseResult<List<DinasProductListDTO>> getOrderProduct(Integer id) {
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,purchaseOrderMapper.getOrderProduct(id));
    }

    /**
     * 根据产品id获取采购订单使用的规格
     * @param orderId
     * @param productId
     * @return
     */
    @Override
    public ResponseResult<List<DinasProductSpecListDTO>> getOrderProductSpec(Integer orderId, Integer productId) {
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,purchaseOrderMapper.getOrderProductSpec(orderId,productId));
    }

    /**
     * 根据产品规格获取调价前价格
     * @param orderId
     * @param productId
     * @param specId
     * @return
     */
    @Override
    public ResponseResult<DinasPurchaseOrderDetail> getPriceByProductSpec(Integer orderId, Integer productId, Integer specId) {
        DinasPurchaseOrderDetail detail = purchaseOrderDetailService.selectOne(new EntityWrapper<>(new DinasPurchaseOrderDetail()
                .setOrderId(orderId).setProductId(productId).setProductSpecId(specId).setDeleted(Status.FALSE.getKey())));
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, detail);
    }

    /**
     * 查询采购订单下拉列表
     * @param userInfo
     * @return
     */
    @Override
    public List<DinasPurchaseOrderPullListDTO> getOrderPullList(AuthPlatformUserInfo userInfo) {
        return purchaseOrderMapper.getOrderPullList(userInfo.getOrgId());
    }
}
