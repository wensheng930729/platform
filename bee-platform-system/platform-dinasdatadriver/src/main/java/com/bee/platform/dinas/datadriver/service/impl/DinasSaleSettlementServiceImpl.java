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
import com.bee.platform.dinas.datadriver.dao.mapper.DinasSaleSettlementMapper;
import com.bee.platform.dinas.datadriver.dto.DinasSaleSettlementSearchDTO;
import com.bee.platform.dinas.datadriver.entity.DinasSaleOrder;
import com.bee.platform.dinas.datadriver.entity.DinasSaleSettlement;
import com.bee.platform.dinas.datadriver.rq.DinasSaleSettlementSearchRQ;
import com.bee.platform.dinas.datadriver.rq.SettlementBatchIdsRQ;
import com.bee.platform.dinas.datadriver.rq.SettlementBatchRQ;
import com.bee.platform.dinas.datadriver.rq.SettlementRQ;
import com.bee.platform.dinas.datadriver.service.DinasSaleOrderService;
import com.bee.platform.dinas.datadriver.service.DinasSaleSettlementService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.ObjectUtils;

import java.math.BigDecimal;
import java.util.Date;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * <p>
 * 销售结算表 服务实现类
 * </p>
 *
 * @author liliang123
 * @since 2019-08-13
 */
@Slf4j
@Service
public class DinasSaleSettlementServiceImpl extends ServiceImpl<DinasSaleSettlementMapper, DinasSaleSettlement> implements DinasSaleSettlementService {

    @Autowired
    private DinasSaleOrderService saleOrderService;

    @Override
    public ResponseResult<List<DinasSaleSettlementSearchDTO>> searchSaleSettlementByCondition(DinasSaleSettlementSearchRQ rq, Page page, Integer companyId) {

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


        List<DinasSaleSettlementSearchDTO> dto = baseMapper.searchSaleSettlementByCondition(rq, pagination);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto, PageUtils.transToPage(pagination));

    }


    @Override
    public List<Integer> cancelSettlementByIds(AuthPlatformUserInfo userInfo, List<Integer> ids) {
        Date time = new Date();
        Integer userId = userInfo.getId();
        List<Integer> idList = ids.stream().distinct().collect(Collectors.toList());
//        Wrapper<DinasSaleSettlement> wrapper = new EntityWrapper<DinasSaleSettlement>().eq("deleted", 0).eq("status", 1);
        for (Integer id : idList) {
            DinasSaleSettlement one = selectOne(new EntityWrapper<DinasSaleSettlement>().eq("deleted", 0).eq("status", 1).eq("id", id));
            if (ObjectUtils.isEmpty(one)) {
                log.error("参数错误，未找到相关数据，撤销结算失败，id为：" + id);
                throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.DINAS_SALE_SETTLEMENT_CANCEL_SETTLEMENT_FAILED);
            }
            // 获取结算总价
            BigDecimal sumPrice = one.getSettlementSumPrice();
            Integer saleOrderId = one.getSaleOrderId();
            // 修改结算单状态 和结算金额置为空
            one.setStatus(0).setUpdateUser(userId).setUpdateTime(time).setSettlementUnitPrice(null).setSettlementSumPrice(null).setSettlementDate(null);
            if (!updateById(one)) {
                log.error("撤销结算失败，id为：" + id);
                throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.DINAS_SALE_SETTLEMENT_CANCEL_SETTLEMENT_FAILED);
            }
            if (ObjectUtils.isEmpty(sumPrice)) {
                log.info("结算总金额为空 ，不进行可用余额加减");
                continue;
            }
            // 查询采购合同单进行可用余额调整
            DinasSaleOrder saleOrder = saleOrderService.selectOne(new EntityWrapper<DinasSaleOrder>().eq("id", saleOrderId).eq("deleted", 0));
            if (ObjectUtils.isEmpty(saleOrder)) {
                log.error("撤销结算失败，没有相关的销售合同单,id为：" + id);
                throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.DINAS_SALE_SETTLEMENT_CANCEL_SETTLEMENT_FAILED_NO_SALE_ORDER);
            }
            BigDecimal availableAmount = saleOrder.getAvailableAmount();
            saleOrder.setAvailableAmount(availableAmount.add(sumPrice)).setUpdateUser(userId).setUpdateTime(time);
            if (!saleOrderService.updateById(saleOrder)) {
                log.error("撤销结算失败，修改销售合同单可用金额失败,id为：" + id);
                throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.DINAS_SALE_SETTLEMENT_CANCEL_SETTLEMENT_FAILED_UPDATE_SALE_ORDER_FAILED);
            }

        }
        return idList;

    }

    @Override
    public Integer settlementOne(AuthPlatformUserInfo userInfo, SettlementRQ rq) {
        Date time = new Date();
        Integer userId = userInfo.getId();
        Integer id = rq.getId();
        BigDecimal unitPrice = rq.getSettlementUnitPrice();
        BigDecimal sumPrice = rq.getSettlementSumPrice();
        Wrapper<DinasSaleSettlement> wrapper = new EntityWrapper<DinasSaleSettlement>().eq("id", id).eq("deleted", 0).eq("status", 0);
        // 查询结算单
        DinasSaleSettlement one = selectOne(wrapper);
        if (ObjectUtils.isEmpty(one)) {
            log.error("参数错误，未找到相关销售结算数据，结算失败，id为：" + id);
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.DINAS_SALE_SETTLEMENT_SETTLEMENT_FAILED_NO_SETTLEMENT_DATA);
        }
        Integer saleOrderId = one.getSaleOrderId();
        // 查询采购合同单
        DinasSaleOrder saleOrder = saleOrderService.selectOne(new EntityWrapper<DinasSaleOrder>().eq("id", saleOrderId).eq("deleted", 0));
        if (ObjectUtils.isEmpty(saleOrder)) {
            log.error("参数错误，未找到相关销售合同数据，结算失败，id为：" + id);
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.DINAS_SALE_SETTLEMENT_SETTLEMENT_FAILED_NO_SALE_DATA);
        }
        BigDecimal availableAmount = saleOrder.getAvailableAmount();
        // 判断金额是否足额
        if (availableAmount.compareTo(sumPrice) < 0) {
            log.error("合同可用余额不足不能结算，结算失败，id为：" + id);
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.DINAS_SALE_SETTLEMENT_SETTLEMENT_FAILED_NO_MONEY);
        }
        // 修改结算单价、总价、结算状态
        one.setSettlementUnitPrice(unitPrice).setSettlementSumPrice(sumPrice).setSettlementDate(time).setStatus(1).setUpdateUser(userId).setUpdateTime(time);
        if (!updateById(one)) {
            log.error("修改销售结算单失败，id为：" + id);
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.DINAS_SALE_SETTLEMENT_SETTLEMENT_FAILED);
        }
        // 调整采购合同可用余额
        saleOrder.setAvailableAmount(availableAmount.subtract(sumPrice)).setUpdateUser(userId).setUpdateTime(time);
        if (!saleOrderService.updateById(saleOrder)) {
            log.error("修改销售合同单失败，id为：" + id);
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.DINAS_SALE_SETTLEMENT_SETTLEMENT_FAILED_SALE_ORDER_FAILED);
        }
        return id;
    }

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
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.DINAS_SALE_SETTLEMENT_SETTLEMENT_BATCH_FAILED);
        }
        BigDecimal unitPrice = rq.getSettlementUnitPrice();
        BigDecimal sumPrice = rq.getSettlementSumPrice();
        // 校验余额是否够用
        Integer saleOrderId = ids.get(0).getContractOrderId();
        // 查询销售合同单
        DinasSaleOrder purchaseOrder = saleOrderService.selectOne(new EntityWrapper<DinasSaleOrder>().eq("id", saleOrderId).eq("deleted", 0));
        if (ObjectUtils.isEmpty(purchaseOrder)) {
            log.error("参数错误，未找到相关销售合同数据，结算失败，id为：" + saleOrderId);
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.DINAS_SALE_SETTLEMENT_SETTLEMENT_FAILED_NO_SALE_DATA);
        }
        BigDecimal availableAmount = purchaseOrder.getAvailableAmount();
        // 判断金额是否足额
        if (availableAmount.compareTo(sumPrice) < 0) {
            log.error("合同可用余额不足不能结算，结算失败，id为：" + saleOrderId);
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.DINAS_SALE_SETTLEMENT_SETTLEMENT_FAILED_NO_MONEY);
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
        DinasSaleOrder saleOrder = saleOrderService.selectOne(new EntityWrapper<DinasSaleOrder>().eq("id", contractId).eq("deleted", 0));
        if (ObjectUtils.isEmpty(saleOrder)) {
            log.info("采购合同不存在，id为：" + contractId);
            return BigDecimal.ZERO;
        }
        return saleOrder.getAvailableAmount();
    }
}
