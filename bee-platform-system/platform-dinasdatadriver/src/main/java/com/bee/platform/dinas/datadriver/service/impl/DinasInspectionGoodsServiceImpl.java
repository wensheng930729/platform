package com.bee.platform.dinas.datadriver.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.mapper.Wrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.dinas.datadriver.dao.mapper.DinasInspectionGoodsMapper;
import com.bee.platform.dinas.datadriver.dto.*;
import com.bee.platform.dinas.datadriver.entity.*;
import com.bee.platform.dinas.datadriver.rq.*;
import com.bee.platform.dinas.datadriver.service.*;
import com.google.common.collect.Lists;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
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
 * 验货磅单表 服务实现类
 * </p>
 *
 * @author liliang123
 * @since 2019-08-13
 */
@Slf4j
@Service
public class DinasInspectionGoodsServiceImpl extends ServiceImpl<DinasInspectionGoodsMapper, DinasInspectionGoods> implements DinasInspectionGoodsService {

    @Autowired
    private DinasPurchaseOrderService purchaseOrderService;

    @Autowired
    private DinasSaleOrderService saleOrderService;

    @Autowired
    private DinasPurchaseSettlementService purchaseSettlementService;

    @Autowired
    private DinasSaleSettlementService saleSettlementService;

    @Autowired
    private DinasProductService productService;

    @Autowired
    private DinasProductSpecService productSpecService;

    @Override
    public ResponseResult<List<DinasInspectionGoodsSearchDTO>> searchInspectionGoodsByCondition(DinasInspectionGoodsSearchRQ rq, Page page, Integer companyId) {
        Pagination pagination = PageUtils.transFromPage(page);
        rq.setCompanyId(companyId);
        String startTime = rq.getStartTime();
        if (!StringUtils.isEmpty(startTime)) {
            rq.setStartTime(startTime + " 00:00:00");
        }
        String endTime = rq.getEndTime();
        if (!StringUtils.isEmpty(endTime)) {
            rq.setEndTime(endTime + " 23:59:59");
        }

        List<DinasInspectionGoodsSearchDTO> dto = baseMapper.searchInspectionGoodsByCondition(rq, pagination);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto, PageUtils.transToPage(pagination));
    }

    @Transactional(rollbackFor = Exception.class)
    @Override
    public Integer saveInspectionGoods(AuthPlatformUserInfo userInfo, DinasInspectionGoodsSaveRQ rq) {
        Date time = new Date();
        Integer companyId = userInfo.getOrgId();
        Integer userId = userInfo.getId();
        DinasInspectionGoods order = BeanUtils.copyProperties(rq, DinasInspectionGoods.class);
//        String url = StringUtils.join(rq.getUrlList(), ",");
        order.setCompanyId(companyId).setCreateUser(userId).setCreateTime(time);
        // 校验采购合同单和销售合同单
        checkPurchaseOrderAndSaleOrder(rq.getPurchaseOrderId(), rq.getSaleOrderId());
        // 保存验货磅单信息
        if (!insert(order)) {
            log.error("保存验货磅单信息失败，调用{}的{}方法出错", "DinasInspectionGoodsServiceImpl", "saveInspectionGoods()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.DINAS_INSPECTION_GOODS_SAVE_FAILED);
        }

        Integer inspectionGoodsId = order.getId();
        // 生成采购结算单和销售结算单
        generatePurchaseAndSaleSettlement(time, companyId, userId, inspectionGoodsId, rq.getPurchaseOrderId(), rq.getSaleOrderId());

        return inspectionGoodsId;
    }

    private void generatePurchaseAndSaleSettlement(Date time, Integer companyId, Integer userId, Integer inspectionGoodsId, Integer purchaseOrderId, Integer saleOrderId) {
        // 生成采购结算单
        DinasPurchaseSettlement purchaseSettlement = new DinasPurchaseSettlement().setCompanyId(companyId).setInspectionOrderId(inspectionGoodsId)
                .setPurchaseOrderId(purchaseOrderId).setCreateUser(userId).setCreateTime(time);

        if (!purchaseSettlementService.insert(purchaseSettlement)) {
            log.error("保存验货磅单信息,生成采购结算单失败，调用{}的{}方法出错", "DinasInspectionGoodsServiceImpl", "saveInspectionGoods()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.DINAS_INSPECTION_GOODS_SAVE_PURCHASE_FAILED);
        }
        // 生成销售结算单
        DinasSaleSettlement saleSettlement = new DinasSaleSettlement().setCompanyId(companyId).setInspectionOrderId(inspectionGoodsId).setSaleOrderId(saleOrderId)
                .setCreateUser(userId).setCreateTime(time);
        if (!saleSettlementService.insert(saleSettlement)) {
            log.error("保存验货磅单信息,生成销售结算单失败，调用{}的{}方法出错", "DinasInspectionGoodsServiceImpl", "saveInspectionGoods()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.DINAS_INSPECTION_GOODS_SAVE_SALE_FAILED);
        }
    }

    private void checkPurchaseOrderAndSaleOrder(Integer purchaseOrderId, Integer saleOrderId) {
        List<DinasPurchaseOrder> purchaseOrders = purchaseOrderService.selectList(new EntityWrapper<DinasPurchaseOrder>().eq("id", purchaseOrderId).eq("deleted", 0));
        List<DinasSaleOrder> saleOrders = saleOrderService.selectList(new EntityWrapper<DinasSaleOrder>().eq("id", saleOrderId).eq("deleted", 0));
        if (purchaseOrders.size() != 1 || saleOrders.size() != 1) {
            log.error("保存验货磅单信息失败，采购合同id和销售合同id数据不存在,调用{}的{}方法出错", "DinasInspectionGoodsServiceImpl", "saveInspectionGoods()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.DINAS_INSPECTION_GOODS_SAVE_FAILED);
        }
    }

    @Transactional(rollbackFor = Exception.class)
    @Override
    public Integer updateInspectionGoods(AuthPlatformUserInfo userInfo, DinasInspectionGoodsUpdateRQ rq) {
        Date time = new Date();
        Integer userId = userInfo.getId();
        Integer companyId = userInfo.getOrgId();
        DinasInspectionGoods order = BeanUtils.copyProperties(rq, DinasInspectionGoods.class);
//        String url = StringUtils.join(rq.getUrlList(), ",");
        order.setUpdateUser(userId).setUpdateTime(time);
        checkPurchaseOrderAndSaleOrder(rq.getPurchaseOrderId(), rq.getSaleOrderId());
        // 查询是否有已结算过的数据
        List<DinasPurchaseSettlement> alreadyPurchaseSettlements = purchaseSettlementService.selectList(new EntityWrapper<DinasPurchaseSettlement>().eq("inspection_order_id", rq.getId()).eq("deleted", 0).eq("status", 1));
        List<DinasSaleSettlement> alreadySaleSettlements = saleSettlementService.selectList(new EntityWrapper<DinasSaleSettlement>().eq("inspection_order_id", rq.getId()).eq("deleted", 0).eq("status", 1));
        if (!CollectionUtils.isEmpty(alreadyPurchaseSettlements) || !CollectionUtils.isEmpty(alreadySaleSettlements)) {
            log.error("删除验货磅单信息失败,数据异常,存在已结算过的数据,调用{}的{}方法出错", "DinasInspectionGoodsServiceImpl", "updateInspectionGoods()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.DINAS_INSPECTION_GOODS_UPDATE_FAILED_HAS_ALREADY);
        }
        // 保存验货磅单信息
        if (!updateById(order)) {
            log.error("保存验货磅单信息失败，调用{}的{}方法出错", "DinasInspectionGoodsServiceImpl", "saveInspectionGoods()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.DINAS_INSPECTION_GOODS_SAVE_FAILED);
        }
        // 删除旧的采购结算单
        Wrapper<DinasPurchaseSettlement> wrapper1 = new EntityWrapper<DinasPurchaseSettlement>().eq("inspection_order_id", rq.getId()).eq("deleted", 0);
        List<DinasPurchaseSettlement> purchaseSettlements = purchaseSettlementService.selectList(wrapper1);
        if (!CollectionUtils.isEmpty(purchaseSettlements)) {
            if (!purchaseSettlementService.update(new DinasPurchaseSettlement().setDeleted(1).setUpdateUser(userId).setUpdateTime(time), wrapper1)) {
                log.error("保存验货磅单信息失败，删除旧的结算单失败,调用{}的{}方法出错", "DinasInspectionGoodsServiceImpl", "saveInspectionGoods()");
                throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.DINAS_INSPECTION_GOODS_SAVE_FAILED_DELETE_OLD_PURCHASE_FAILED);
            }
        }
        // 删除旧的销售结算单
        Wrapper<DinasSaleSettlement> wrapper2 = new EntityWrapper<DinasSaleSettlement>().eq("inspection_order_id", rq.getId()).eq("deleted", 0);
        List<DinasSaleSettlement> saleSettlements = saleSettlementService.selectList(wrapper2);
        if (!CollectionUtils.isEmpty(saleSettlements)) {
            if (!saleSettlementService.update(new DinasSaleSettlement().setDeleted(1).setUpdateUser(userId).setUpdateTime(time), wrapper2)) {
                log.error("保存验货磅单信息失败，删除旧的结算单失败,调用{}的{}方法出错", "DinasInspectionGoodsServiceImpl", "saveInspectionGoods()");
                throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.DINAS_INSPECTION_GOODS_SAVE_FAILED_DELETE_OLD_PURCHASE_FAILED);
            }
        }
        Integer inspectionGoodsId = order.getId();
        // 生成采购结算单和销售结算单
        generatePurchaseAndSaleSettlement(time, companyId, userId, inspectionGoodsId, rq.getPurchaseOrderId(), rq.getSaleOrderId());

        return order.getId();
    }


    @Override
    public DinasInspectionGoodsDTO getInspectionGoodsById(Integer id) {
        // 根据id查询验货磅单信息
        DinasInspectionGoods order = selectById(id);
        DinasInspectionGoodsDTO dto = BeanUtils.copyProperties(order, DinasInspectionGoodsDTO.class);
//        if(StringUtils.isNotEmpty(order.getUrl())){
//            String[] urls = order.getUrl().split(",");
//            dto.setUrlList(Lists.newArrayList(urls));
//        }
        // 根据采购合同id查询采购合同信息
        DinasPurchaseOrder purchaseOrder = purchaseOrderService.selectById(order.getPurchaseOrderId());
        // 根据销售合同id查询销售合同信息
        DinasSaleOrder saleOrder = saleOrderService.selectById(order.getSaleOrderId());
        // 根据产品id查询产品信息
        DinasProduct product = productService.selectById(order.getProductId());
        // 根据产品规格id查询产品规格信息
        DinasProductSpec productSpec = productSpecService.selectById(order.getProductSpecId());
        if (!ObjectUtils.isEmpty(purchaseOrder)) {
            dto.setPurchaseOrderCode(purchaseOrder.getCode());
        }
        if (!ObjectUtils.isEmpty(saleOrder)) {
            dto.setSaleOrderCode(saleOrder.getCode());
        }
        if (!ObjectUtils.isEmpty(product)) {
            dto.setProductName(product.getProductName());
        }
        if (!ObjectUtils.isEmpty(productSpec)) {
            dto.setProductSpecName(productSpec.getSpecName());
        }

        return dto;
    }

    @Transactional(rollbackFor = Exception.class)
    @Override
    public List<Integer> deleteInspectionGoodsByIds(AuthPlatformUserInfo userInfo, List<Integer> ids) {
        Date time = new Date();
        Integer userId = userInfo.getId();
        List<Integer> idList = ids.stream().distinct().collect(Collectors.toList());
        // 查询是否有已结算过的数据
        List<DinasPurchaseSettlement> alreadyPurchaseSettlements = purchaseSettlementService.selectList(new EntityWrapper<DinasPurchaseSettlement>().in("inspection_order_id", idList).eq("deleted", 0).eq("status", 1));
        List<DinasSaleSettlement> alreadySaleSettlements = saleSettlementService.selectList(new EntityWrapper<DinasSaleSettlement>().in("inspection_order_id", idList).eq("deleted", 0).eq("status", 1));
        if (!CollectionUtils.isEmpty(alreadyPurchaseSettlements) || !CollectionUtils.isEmpty(alreadySaleSettlements)) {
            log.error("删除验货磅单信息失败,数据异常,存在已结算过的数据,调用{}的{}方法出错", "DinasInspectionGoodsServiceImpl", "deleteInspectionGoodsByIds()");
            throw new BusinessException(ResCodeEnum.DELETE_FAILED, ExceptionMessageEnum.DINAS_INSPECTION_GOODS_DELETED_FAILED_HAS_ALREADY);
        }
        Wrapper<DinasInspectionGoods> wrapper1 = new EntityWrapper<DinasInspectionGoods>().in("id", idList).eq("deleted", 0);
        Wrapper<DinasPurchaseSettlement> wrapper2 = new EntityWrapper<DinasPurchaseSettlement>().in("inspection_order_id", idList).eq("deleted", 0);
        Wrapper<DinasSaleSettlement> wrapper3 = new EntityWrapper<DinasSaleSettlement>().in("inspection_order_id", idList).eq("deleted", 0);
        List<DinasInspectionGoods> inspectionGoodsList = selectList(wrapper1);
        List<DinasPurchaseSettlement> purchaseSettlementList = purchaseSettlementService.selectList(wrapper2);
        List<DinasSaleSettlement> saleSettlementList = saleSettlementService.selectList(wrapper3);
        if (CollectionUtils.isEmpty(inspectionGoodsList)) {
            log.info("没有验货磅单数据");
            throw new BusinessException(ResCodeEnum.DELETE_FAILED, ExceptionMessageEnum.DINAS_INSPECTION_GOODS_DELETED_FAILED_NO_DATA);
        }
        // 删除验货磅单
        if (!update(new DinasInspectionGoods().setDeleted(1).setUpdateUser(userId).setUpdateTime(time), wrapper1)) {
            log.error("删除验货磅单信息失败,调用{}的{}方法出错", "DinasInspectionGoodsServiceImpl", "deleteInspectionGoodsByIds()");
            throw new BusinessException(ResCodeEnum.DELETE_FAILED, ExceptionMessageEnum.DINAS_INSPECTION_GOODS_DELETED_FAILED);
        }
        // 删除采购结算单
        if (!CollectionUtils.isEmpty(purchaseSettlementList)) {
            if (!purchaseSettlementService.update(new DinasPurchaseSettlement().setDeleted(1).setUpdateUser(userId).setUpdateTime(time), wrapper2)) {
                log.error("删除验货磅单信息失败,删除采购结算失败,调用{}的{}方法出错", "DinasInspectionGoodsServiceImpl", "deleteInspectionGoodsByIds()");
                throw new BusinessException(ResCodeEnum.DELETE_FAILED, ExceptionMessageEnum.DINAS_INSPECTION_GOODS_DELETED_PURCHASE_FAILED);
            }
        }

        // 删除销售结算单
        if (!CollectionUtils.isEmpty(saleSettlementList)) {
            if (!saleSettlementService.update(new DinasSaleSettlement().setDeleted(1).setUpdateUser(userId).setUpdateTime(time), wrapper3)) {
                log.error("删除验货磅单信息失败,删除销售结算失败,调用{}的{}方法出错", "DinasInspectionGoodsServiceImpl", "deleteInspectionGoodsByIds()");
                throw new BusinessException(ResCodeEnum.DELETE_FAILED, ExceptionMessageEnum.DINAS_INSPECTION_GOODS_DELETED_SALE_FAILED);
            }
        }

        return idList;

    }


    @Override
    public List<DinasPurchaseCodeListDTO> getPurchaseCodeList(Integer companyId) {
        List<DinasPurchaseCodeListDTO> dto = Lists.newArrayList();
        List<DinasPurchaseOrder> purchaseOrders = purchaseOrderService.selectList(new EntityWrapper<DinasPurchaseOrder>().eq("company_id", companyId).eq("deleted", 0));
        if (CollectionUtils.isEmpty(purchaseOrders)) {
            log.info("采购合同列表为空");
            return dto;
        }
        for (DinasPurchaseOrder purchaseOrder : purchaseOrders) {
            dto.add(new DinasPurchaseCodeListDTO().setPurchaseOrderId(purchaseOrder.getId()).setPurchaseOrderCode(purchaseOrder.getCode()));
        }
        return dto;
    }


    @Override
    public List<DinasSaleCodeListDTO> getSaleCodeList(Integer companyId) {
        List<DinasSaleCodeListDTO> dto = Lists.newArrayList();
        List<DinasSaleOrder> saleOrders = saleOrderService.selectList(new EntityWrapper<DinasSaleOrder>().eq("company_id", companyId).eq("deleted", 0));
        if (CollectionUtils.isEmpty(saleOrders)) {
            log.info("采购合同列表为空");
            return dto;
        }
        for (DinasSaleOrder saleOrder : saleOrders) {
            dto.add(new DinasSaleCodeListDTO().setSaleOrderId(saleOrder.getId()).setSaleOrderCode(saleOrder.getCode()));
        }
        return dto;
    }


    @Override
    public List<DinasProductListDTO> getProductListByPurchaseIdAndSaleId(DinasGetProductListRQ rq) {

        return baseMapper.getProductList(rq);
    }

    @Override
    public List<DinasProductSpecListDTO> getProductSpecListByPurchaseIdAndSaleIdAndProductId(DinasGetProductSpecListRQ rq) {
        return baseMapper.getProductSpecList(rq);
    }
}
