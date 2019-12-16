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
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.dinas.datadriver.dao.mapper.DinasPurchaseSettlementMapper;
import com.bee.platform.dinas.datadriver.dto.DinasPurchaseSettlementSearchDTO;
import com.bee.platform.dinas.datadriver.entity.DinasPurchaseOrder;
import com.bee.platform.dinas.datadriver.entity.DinasPurchaseSettlement;
import com.bee.platform.dinas.datadriver.rq.DinasPurchaseSettlementSearchRQ;
import com.bee.platform.dinas.datadriver.rq.SettlementBatchIdsRQ;
import com.bee.platform.dinas.datadriver.rq.SettlementBatchRQ;
import com.bee.platform.dinas.datadriver.rq.SettlementRQ;
import com.bee.platform.dinas.datadriver.service.DinasPurchaseOrderService;
import com.bee.platform.dinas.datadriver.service.DinasPurchaseSettlementService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ObjectUtils;

import java.math.BigDecimal;
import java.util.Date;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * <p>
 * 采购结算表 服务实现类
 * </p>
 *
 * @author liliang123
 * @since 2019-08-13
 */
@Slf4j
@Service
public class DinasPurchaseSettlementServiceImpl extends ServiceImpl<DinasPurchaseSettlementMapper, DinasPurchaseSettlement> implements DinasPurchaseSettlementService {

    @Autowired
    private DinasPurchaseOrderService purchaseOrderService;

    @Override
    public ResponseResult<List<DinasPurchaseSettlementSearchDTO>> searchPurchaseSettlementByCondition(DinasPurchaseSettlementSearchRQ rq, Page page, Integer companyId) {
        Pagination pagination = PageUtils.transFromPage(page);
        rq.setCompanyId(companyId);
        String inspectionStartTime = rq.getInspectionStartTime();
        if (!StringUtils.isEmpty(inspectionStartTime)) {
            rq.setInspectionStartTime(inspectionStartTime + " 00:00:00");
        }
        String inspectionEndTime = rq.getInspectionEndTime();
        if (!StringUtils.isEmpty(inspectionEndTime)) {
            rq.setInspectionEndTime(inspectionEndTime + " 23:59:59");
        }

        String settlementStartTime = rq.getSettlementStartTime();
        if (!StringUtils.isEmpty(settlementStartTime)) {
            rq.setSettlementStartTime(settlementStartTime + " 00:00:00");
        }
        String settlementEndTime = rq.getSettlementEndTime();
        if (!StringUtils.isEmpty(settlementEndTime)) {
            rq.setSettlementEndTime(settlementEndTime + " 23:59:59");
        }

        List<DinasPurchaseSettlementSearchDTO> dto = baseMapper.searchPurchaseSettlementByCondition(rq, pagination);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto, PageUtils.transToPage(pagination));

    }

    @Transactional(rollbackFor = Exception.class)
    @Override
    public List<Integer> cancelSettlementByIds(AuthPlatformUserInfo userInfo, List<Integer> ids) {
        Date time = new Date();
        Integer userId = userInfo.getId();
        List<Integer> idList = ids.stream().distinct().collect(Collectors.toList());
//        Wrapper<DinasPurchaseSettlement> wrapper = new EntityWrapper<DinasPurchaseSettlement>().eq("deleted", 0).eq("status", 1);
        for (Integer id : idList) {
            DinasPurchaseSettlement one = selectOne(new EntityWrapper<DinasPurchaseSettlement>().eq("deleted", 0).eq("status", 1).eq("id", id));
            if (ObjectUtils.isEmpty(one)) {
                log.error("参数错误，未找到相关数据，撤销结算失败，id为：" + id);
                throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.DINAS_PURCHASE_SETTLEMENT_CANCEL_SETTLEMENT_FAILED);
            }
            // 获取结算总价
            BigDecimal sumPrice = one.getSettlementSumPrice();
            Integer purchaseOrderId = one.getPurchaseOrderId();
            // 修改结算单状态 和结算金额，结算日期 置为空
            one.setStatus(0).setUpdateUser(userId).setUpdateTime(time).setSettlementUnitPrice(null).setSettlementSumPrice(null).setSettlementDate(null);
            if (!updateById(one)) {
                log.error("撤销结算失败，id为：" + id);
                throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.DINAS_PURCHASE_SETTLEMENT_CANCEL_SETTLEMENT_FAILED);
            }
            if (ObjectUtils.isEmpty(sumPrice)) {
                log.info("结算总金额为空 ，不进行可用余额加减");
                continue;
            }
            // 查询采购合同单进行可用余额调整
            DinasPurchaseOrder purchaseOrder = purchaseOrderService.selectOne(new EntityWrapper<DinasPurchaseOrder>().eq("id", purchaseOrderId).eq("deleted", 0));
            if (ObjectUtils.isEmpty(purchaseOrder)) {
                log.error("撤销结算失败，没有相关的采购合同单,id为：" + id);
                throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.DINAS_PURCHASE_SETTLEMENT_CANCEL_SETTLEMENT_FAILED_NO_PURCHASE_ORDER);
            }
            BigDecimal availableAmount = purchaseOrder.getAvailableAmount();
            purchaseOrder.setAvailableAmount(availableAmount.add(sumPrice)).setUpdateUser(userId).setUpdateTime(time);
            if (!purchaseOrderService.updateById(purchaseOrder)) {
                log.error("撤销结算失败，修改采购合同单可用金额失败,id为：" + id);
                throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.DINAS_PURCHASE_SETTLEMENT_CANCEL_SETTLEMENT_FAILED_UPDATE_PURCHASE_ORDER_FAILED);
            }

        }

        return idList;


    }

    @Transactional(rollbackFor = Exception.class)
    @Override
    public Integer settlementOne(AuthPlatformUserInfo userInfo, SettlementRQ rq) {
        Date time = new Date();
        Integer userId = userInfo.getId();
        Integer id = rq.getId();
        BigDecimal unitPrice = rq.getSettlementUnitPrice();
        BigDecimal sumPrice = rq.getSettlementSumPrice();
        Wrapper<DinasPurchaseSettlement> wrapper = new EntityWrapper<DinasPurchaseSettlement>().eq("id", id).eq("deleted", 0).eq("status", 0);
        // 查询结算单
        DinasPurchaseSettlement one = selectOne(wrapper);
        if (ObjectUtils.isEmpty(one)) {
            log.error("参数错误，未找到相关采购结算数据，结算失败，id为：" + id);
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.DINAS_PURCHASE_SETTLEMENT_SETTLEMENT_FAILED_NO_SETTLEMENT_DATA);
        }
        Integer purchaseOrderId = one.getPurchaseOrderId();
        // 查询采购合同单
        DinasPurchaseOrder purchaseOrder = purchaseOrderService.selectOne(new EntityWrapper<DinasPurchaseOrder>().eq("id", purchaseOrderId).eq("deleted", 0));
        if (ObjectUtils.isEmpty(purchaseOrder)) {
            log.error("参数错误，未找到相关采购合同数据，结算失败，id为：" + id);
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.DINAS_PURCHASE_SETTLEMENT_SETTLEMENT_FAILED_NO_PURCHASE_DATA);
        }
        BigDecimal availableAmount = purchaseOrder.getAvailableAmount();
        // 判断金额是否足额
        if (availableAmount.compareTo(sumPrice) < 0) {
            log.error("合同可用余额不足不能结算，结算失败，id为：" + id);
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.DINAS_PURCHASE_SETTLEMENT_SETTLEMENT_FAILED_NO_MONEY);
        }
        // 修改结算单价、总价、结算状态
        one.setSettlementUnitPrice(unitPrice).setSettlementSumPrice(sumPrice).setSettlementDate(time).setStatus(1).setUpdateUser(userId).setUpdateTime(time);
        if (!updateById(one)) {
            log.error("修改采购结算单失败，id为：" + id);
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.DINAS_PURCHASE_SETTLEMENT_SETTLEMENT_FAILED);
        }
        // 调整采购合同可用余额
        purchaseOrder.setAvailableAmount(availableAmount.subtract(sumPrice)).setUpdateUser(userId).setUpdateTime(time);
        if (!purchaseOrderService.updateById(purchaseOrder)) {
            log.error("修改采购合同单失败，id为：" + id);
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.DINAS_PURCHASE_SETTLEMENT_SETTLEMENT_FAILED_PURCHASE_ORDER_FAILED);
        }
        return id;
    }

    @Transactional(rollbackFor = Exception.class)
    @Override
    public List<Integer> settlementBatch(AuthPlatformUserInfo userInfo, SettlementBatchRQ rq) {
        List<SettlementBatchIdsRQ> ids = rq.getIds().stream().distinct().collect(Collectors.toList());
        List<Integer> orderIds = ids.stream().map(o -> o.getId()).distinct().collect(Collectors.toList());
        Set<Integer> contractOrderIds = ids.stream().map(o -> o.getContractOrderId()).collect(Collectors.toSet());
        Set<Integer> productIds = ids.stream().map(o -> o.getProductId()).collect(Collectors.toSet());
        Set<Integer> productSpecIds = ids.stream().map(o -> o.getProductSpecId()).collect(Collectors.toSet());
        // 校验是否是 同一合同号，同一产品名称、同一规格
        if (contractOrderIds.size() > 1 || productIds.size() > 1 || productSpecIds.size() > 1) {
            log.error("批量结算失败,同一合同号，同一产品名称、同一规格才能批量结算");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.DINAS_PURCHASE_SETTLEMENT_SETTLEMENT_BATCH_FAILED);
        }
        BigDecimal unitPrice = rq.getSettlementUnitPrice();
        BigDecimal sumPrice = rq.getSettlementSumPrice();
        // 校验余额是否够用
        Integer purchaseOrderId = ids.get(0).getContractOrderId();
        // 查询采购合同单
        DinasPurchaseOrder purchaseOrder = purchaseOrderService.selectOne(new EntityWrapper<DinasPurchaseOrder>().eq("id", purchaseOrderId).eq("deleted", 0));
        if (ObjectUtils.isEmpty(purchaseOrder)) {
            log.error("参数错误，未找到相关采购合同数据，结算失败，id为：" + purchaseOrderId);
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.DINAS_PURCHASE_SETTLEMENT_SETTLEMENT_FAILED_NO_PURCHASE_DATA);
        }
        BigDecimal availableAmount = purchaseOrder.getAvailableAmount();
        // 判断金额是否足额
        if (availableAmount.compareTo(sumPrice) < 0) {
            log.error("合同可用余额不足不能结算，结算失败，id为：" + purchaseOrderId);
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.DINAS_PURCHASE_SETTLEMENT_SETTLEMENT_FAILED_NO_MONEY);
        }
        // 依次结算
        for (SettlementBatchIdsRQ batchId : ids) {
            SettlementRQ settlementRQ = new SettlementRQ().setId(batchId.getId()).setSettlementUnitPrice(unitPrice).setSettlementSumPrice(unitPrice.multiply(batchId.getNum()));
            settlementOne(userInfo, settlementRQ);
        }

        return orderIds;
    }

    @Override
    public BigDecimal getAvailableAmount(Integer contractId) {
        DinasPurchaseOrder purchaseOrder = purchaseOrderService.selectOne(new EntityWrapper<DinasPurchaseOrder>().eq("id", contractId).eq("deleted", 0));
        if (ObjectUtils.isEmpty(purchaseOrder)) {
            log.info("采购合同不存在，id为：" + contractId);
            return BigDecimal.ZERO;
        }
        return purchaseOrder.getAvailableAmount();
    }
}
