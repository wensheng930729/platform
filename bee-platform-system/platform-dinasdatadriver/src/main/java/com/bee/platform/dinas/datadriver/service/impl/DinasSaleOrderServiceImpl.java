package com.bee.platform.dinas.datadriver.service.impl;

import com.alibaba.fastjson.JSONObject;
import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.dinas.datadriver.dao.mapper.*;
import com.bee.platform.dinas.datadriver.dto.*;
import com.bee.platform.dinas.datadriver.entity.*;
import com.bee.platform.dinas.datadriver.rq.SaleBatchDeleteRQ;
import com.bee.platform.dinas.datadriver.rq.SaleOrderDetailRQ;
import com.bee.platform.dinas.datadriver.rq.SaleOrderListRQ;
import com.bee.platform.dinas.datadriver.rq.SaleOrderRQ;
import com.bee.platform.dinas.datadriver.service.DinasSaleAdjustService;
import com.bee.platform.dinas.datadriver.service.DinasSaleOrderDetailService;
import com.bee.platform.dinas.datadriver.service.DinasSaleOrderService;
import com.bee.platform.dinas.datadriver.utils.AppendixUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import java.util.*;

/**
 * <p>
 * 销售合同 服务实现类
 * </p>
 *
 * @author huangxin123
 * @since 2019-08-13
 */
@Slf4j
@Service
public class DinasSaleOrderServiceImpl extends ServiceImpl<DinasSaleOrderMapper, DinasSaleOrder> implements DinasSaleOrderService {

    @Autowired
    private DinasSaleOrderDetailService saleOrderDetailService;

    @Autowired
    private DinasSaleAdjustService saleAdjustService;

    @Autowired
    private DinasSaleOrderMapper saleOrderMapper;

    @Autowired
    private DinasSaleOrderDetailMapper saleOrderDetailMapper;

    @Autowired
    private DinasSalePaymentMapper salePaymentMapper;

    @Autowired
    private DinasSaleSettlementMapper saleSettlementMapper;

    @Autowired
    private DinasInspectionGoodsMapper inspectionGoodsMapper;

    @Autowired
    private AppendixUtils appendixUtils;

    @Autowired
    private DinasSaleAdjustDetailMapper adjustDetailMapper;

    /**
     * @descriptin 添加销售合同
     * @author xin.huang
     * @param userInfo
     * @param rq
     * @date 2019/8/14
     * @return
     */
    @Override
    public ResponseResult<Integer> add(AuthPlatformUserInfo userInfo, SaleOrderRQ rq) {
        //校验合同编号是否重复
        if (StringUtils.isNotBlank(rq.getCode())) {
            DinasSaleOrder dinasSaleOrder = this.selectOne(new EntityWrapper<>(new DinasSaleOrder()
                    .setCode(rq.getCode())
                    .setCompanyId(userInfo.getOrgId())
                    .setDeleted(Status.FALSE.getKey())));
            if (Objects.nonNull(dinasSaleOrder)) {
                return ResponseResult.buildResponseResult(ResCodeEnum.DINAS_SALE_ORDER_EXIST);
            }
        }
        //校验产品和规格是否重复
        List<SaleOrderDetailRQ> saleOrderDetailList = rq.getSaleOrderDetailList();
        if (!CollectionUtils.isEmpty(saleOrderDetailList)) {
            Map<Integer, Integer> productMap = new HashMap<>(saleOrderDetailList.size());
            for (SaleOrderDetailRQ order : saleOrderDetailList) {
                Integer specId = productMap.get(order.getProductId());
                if (Objects.nonNull(specId) && specId.equals(order.getProductSpecId())) {
                    return ResponseResult.buildResponseResult(ResCodeEnum.DINAS_SALE_ORDER_PRODUCT_REPEAT);
                }
                productMap.put(order.getProductId(), order.getProductSpecId());
            }
        }
        //添加销售合同基本信息
        DinasSaleOrder dinasSaleOrder = BeanUtils.copyProperties(rq, DinasSaleOrder.class);
        dinasSaleOrder.setCreateTime(new Date())
                .setCreateUser(userInfo.getId())
                .setCompanyId(userInfo.getOrgId())
                .setCompanyName(userInfo.getOrg_name())
                .setUrl(appendixUtils.getJsonStr(rq.getUrlList()));
        if (!this.insert(dinasSaleOrder)) {
            log.error("添加销售合同基本信息失败,调用{}类{}方法出错","DinasSaleOrderServiceImpl","add()");
            return ResponseResult.buildResponseResult(ResCodeEnum.SAVE_FAIL);
        }
         batchInsertDetail(dinasSaleOrder.getId(), rq.getSaleOrderDetailList());

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dinasSaleOrder.getId());
    }

    /**
     * @descriptin 编辑销售合同
     * @author xin.huang
     * @param userInfo
     * @param rq
     * @date 2019/8/14
     * @return
     */
    @Override
    public ResponseResult<Integer> update(AuthPlatformUserInfo userInfo, SaleOrderRQ rq) {
        if (ObjectUtils.isEmpty(rq.getId())) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SALE_ORDER_ID_EMPTY);
        }
        DinasSaleOrder saleOrder = this.selectById(rq.getId());
        if (ObjectUtils.isEmpty(saleOrder)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NO_DATA);
        }

        //校验合同编号是否重复
        if (StringUtils.isNotBlank(rq.getCode())) {
            DinasSaleOrder dinasSaleOrder = this.selectOne(new EntityWrapper<>(new DinasSaleOrder()
                    .setCode(rq.getCode())
                    .setCompanyId(userInfo.getOrgId())
                    .setDeleted(Status.FALSE.getKey())));
            if (Objects.nonNull(dinasSaleOrder) && !rq.getId().equals(dinasSaleOrder.getId())) {
                return ResponseResult.buildResponseResult(ResCodeEnum.DINAS_SALE_ORDER_EXIST);
            }
        }

        //校验产品和规格是否重复
        List<SaleOrderDetailRQ> saleOrderDetailList = rq.getSaleOrderDetailList();
        if (!CollectionUtils.isEmpty(saleOrderDetailList)) {
            Map<Integer, Integer> productMap = new HashMap<>(saleOrderDetailList.size());
            for (SaleOrderDetailRQ order : saleOrderDetailList) {
                Integer specId = productMap.get(order.getProductId());
                if (Objects.nonNull(specId) && specId.equals(order.getProductSpecId())) {
                    return ResponseResult.buildResponseResult(ResCodeEnum.DINAS_SALE_ORDER_PRODUCT_REPEAT);
                }
                productMap.put(order.getProductId(), order.getProductSpecId());
            }
        }

        //编辑销售合同基本信息
        DinasSaleOrder dinasSaleOrder = BeanUtils.copyProperties(rq, DinasSaleOrder.class);
        dinasSaleOrder.setUpdateTime(new Date()).setUpdateUser(userInfo.getId())
                .setUrl(appendixUtils.getJsonStr(rq.getUrlList()));
        if (!this.updateById(dinasSaleOrder)) {
            log.error("编辑销售合同基本信息失败,调用{}类{}方法出错","DinasSaleOrderServiceImpl","update()");
            return ResponseResult.buildResponseResult(ResCodeEnum.SAVE_FAIL);
        }
        //根据销售合同id,删除原有销售合同明细
        saleOrderDetailService.update(new DinasSaleOrderDetail().setDeleted(Status.TRUE.getKey()),
                new EntityWrapper<>(new DinasSaleOrderDetail().setOrderId(rq.getId())));
        batchInsertDetail(rq.getId(), rq.getSaleOrderDetailList());

        //编辑调价函
        saleAdjustService.update(new DinasSaleAdjust().setDeleted(Status.TRUE.getKey()),
                new EntityWrapper<>(new DinasSaleAdjust().setOrderId(rq.getId())));
        if (!CollectionUtils.isEmpty(rq.getSaleAdjustList())) {
            rq.getSaleAdjustList().forEach(adjust -> {
                saleAdjustService.add(userInfo, adjust);
            });
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, rq.getId());
    }

    /**
     * @descriptin 根据合同id查询销售合同详情
     * @author xin.huang
     * @param id
     * @date 2019/8/14
     * @return
     */
    @Override
    public ResponseResult<SaleOrderDTO> findSaleOrderInfo(Integer id) {
        //查询合同基本信息
        SaleOrderDTO saleOrderInfo = saleOrderMapper.findInfo(id);
        if (!ObjectUtils.isEmpty(saleOrderInfo)) {
            if (StringUtils.isNotBlank(saleOrderInfo.getUrl())) {
                saleOrderInfo.setUrlList(JSONObject.parseArray(saleOrderInfo.getUrl(), DinasUrlDTO.class));
            }
            //查询合同明细信息
            saleOrderInfo.setSaleOrderDetailList(saleOrderDetailMapper.findInfoByOrderId(id));

            //查询调价函
            List<DinasSaleAdjust> saleAdjusts = saleAdjustService
                    .selectList(new EntityWrapper<>(new DinasSaleAdjust()
                    .setOrderId(id).setDeleted(Status.FALSE.getKey())));
            if (!CollectionUtils.isEmpty(saleAdjusts)) {
                List<SaleAdjustDTO> saleAdjustList = BeanUtils.assemble(SaleAdjustDTO.class, saleAdjusts);
                saleAdjustList.forEach(adjust -> {
                    if (StringUtils.isNotBlank(adjust.getUrl())) {
                        adjust.setUrlList(JSONObject.parseArray(adjust.getUrl(), DinasUrlDTO.class));
                    }
                    adjust.setSaleAdjustDetailList(adjustDetailMapper.findList(adjust.getId()));
                });
                saleOrderInfo.setSaleAdjustList(saleAdjustList);
            }

        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, saleOrderInfo);
    }

    /**
     * @descriptin 批量删除销售合同
     * @author xin.huang
     * @param rq
     * @date 2019/8/14
     * @return
     */
    @Override
    public ResponseResult<List<Integer>> batchDelete(AuthPlatformUserInfo userInfo, SaleBatchDeleteRQ rq) {
        //查询是否存在销售回款
        List<DinasSalePayment> dinasSalePayments = salePaymentMapper
                .selectList(new EntityWrapper<DinasSalePayment>()
                .in("order_id", rq.getIds())
                .and()
                .eq("deleted", Status.FALSE.getKey()));
        if (!CollectionUtils.isEmpty(dinasSalePayments)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SALE_ORDER_EXIST_PAYMENT);
        }
        //查询是否存在销售结算
        List<DinasSaleSettlement> saleSettlements = saleSettlementMapper
                .selectList(new EntityWrapper<DinasSaleSettlement>()
                .in("sale_order_id", rq.getIds())
                .and()
                .eq("deleted", Status.FALSE.getKey()));
        if (!CollectionUtils.isEmpty(saleSettlements)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SALE_ORDER_EXIST_SETTLEMENT);
        }
        //查询是否存在验货
        List<DinasInspectionGoods> inspectionGoods = inspectionGoodsMapper
                .selectList(new EntityWrapper<DinasInspectionGoods>()
                        .in("sale_order_id", rq.getIds())
                        .and()
                        .eq("deleted", Status.FALSE.getKey()));
        if (!CollectionUtils.isEmpty(inspectionGoods)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SALE_ORDER_EXIST_INSPECTION_GOODS);
        }

        DinasSaleOrder dinasSaleOrder = new DinasSaleOrder()
                .setDeleted(Status.TRUE.getKey())
                .setUpdateTime(new Date())
                .setUpdateUser(userInfo.getId());
        this.update(dinasSaleOrder, new EntityWrapper<DinasSaleOrder>().in("id", rq.getIds()));

        //删除销售合同明细
        saleOrderDetailMapper.update(new DinasSaleOrderDetail().setDeleted(Status.TRUE.getKey()),
                new EntityWrapper<DinasSaleOrderDetail>().in("order_id", rq.getIds()));

        //删除调价
        saleAdjustService.update(new DinasSaleAdjust().setDeleted(Status.TRUE.getKey()),
                new EntityWrapper<DinasSaleAdjust>().in("order_id", rq.getIds()));
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, rq.getIds());
    }

    /**
     * @descriptin 查询销售合同列表
     * @author xin.huang
     * @param userInfo
     * @param rq
     * @param pagination
     * @date 2019/8/14
     * @return
     */
    @Override
    public ResponseResult<List<SaleOrderListDTO>> findSaleOrders(AuthPlatformUserInfo userInfo, SaleOrderListRQ rq, Pagination pagination) {
        if (Objects.isNull(rq)) {
            rq = new SaleOrderListRQ();
        }
        rq.setCompanyId(userInfo.getOrgId());
        List<SaleOrderListDTO> saleOrders = saleOrderMapper.findSaleOrders(rq, pagination);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, saleOrders, PageUtils.transToPage(pagination));
    }

    /**
     * @descriptin 根据公司id获取销售合同列表
     * @author xin.huang
     * @param companyId
     * @date 2019/8/15
     * @return
     */
    @Override
    public ResponseResult<List<SaleOrderDTO>> findOrdersByCompanyId(Integer companyId) {
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, saleOrderMapper.findOrdersByCompanyId(companyId));
    }

    /**
     * @descriptin 根据销售合同id查询当前合同下的产品及对应的规格
     * @author xin.huang
     * @param id
     * @date 2019/8/15
     * @return
     */
    @Override
    public ResponseResult<List<SaleOrderDetailDTO>> findProductsById(Integer id) {
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, saleOrderMapper.findProductsById(id));
    }

    /**
     * @descriptin 根据销售合同id及产品id查询当前产品下的规格
     * @author xin.huang
     * @param orderId
     * @param productId
     * @date 2019/8/15
     * @return
     */
    @Override
    public ResponseResult<List<SaleOrderDetailDTO>> findSpecsByProductId(Integer orderId, Integer productId) {
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, saleOrderMapper.findSpecsByProductId(orderId, productId));
    }

    /**
     * @descriptin 根据产品id及规格id查询调价前价格
     * @author xin.huang
     * @param orderId
     * @param productId
     * @param productSpecId
     * @date 2019/8/15
     * @return
     */
    @Override
    public ResponseResult<SaleOrderDetailDTO> getPriceByProductSpec(Integer orderId, Integer productId, Integer productSpecId) {
        DinasSaleOrderDetail saleOrderDetail = saleOrderDetailService.selectOne(new EntityWrapper<>(new DinasSaleOrderDetail()
                .setOrderId(orderId).setProductId(productId).setProductSpecId(productSpecId)
                .setDeleted(Status.FALSE.getKey())));
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, BeanUtils.copyProperties(saleOrderDetail, SaleOrderDetailDTO.class));
    }

    /**
     * @descriptin 批量添加销售合同明细信息
     * @author xin.huang
     * @param orderId
     * @param saleOrderDetailList
     * @date 2019/8/14
     * @return
     */
    private void batchInsertDetail(Integer orderId, List<SaleOrderDetailRQ> saleOrderDetailList) {
        if (!CollectionUtils.isEmpty(saleOrderDetailList)) {
            List<DinasSaleOrderDetail> saleOrderDetails = BeanUtils.assemble(DinasSaleOrderDetail.class, saleOrderDetailList);
            saleOrderDetails.forEach(order -> {
                order.setOrderId(orderId).setCreateTime(new Date());
            });
            saleOrderDetailService.insertBatch(saleOrderDetails);
        }
    }
}
