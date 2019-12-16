package com.bee.platform.datadriver.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.datadriver.dao.mapper.ErpReceiptOrderDetailMapper;
import com.bee.platform.datadriver.dto.ErpReceiptOrderDetailDTO;
import com.bee.platform.datadriver.entity.ErpReceiptOrderDetail;
import com.bee.platform.datadriver.entity.ErpSaleOrder;
import com.bee.platform.datadriver.rq.ErpReceiptOrderDetailRQ;
import com.bee.platform.datadriver.service.ErpReceiptOrderDetailService;
import com.bee.platform.datadriver.service.ErpSaleOrderService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ObjectUtils;

import java.math.BigDecimal;
import java.util.Date;
import java.util.List;

/**
 * <p>
 * 销售收款单收款详情表 服务实现类
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-28
 */

@Slf4j
@Service
public class ErpReceiptOrderDetailServiceImpl extends ServiceImpl<ErpReceiptOrderDetailMapper, ErpReceiptOrderDetail> implements ErpReceiptOrderDetailService {

    @Autowired
    private ErpSaleOrderService erpSaleOrderService;

    @Autowired
    private ErpReceiptOrderDetailMapper receiptOrderDetailMapper;


    /**
     * 根据id删除销售收款详情
     *
     * @param userInfo
     * @param id
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public void deleteDetail(AuthPlatformUserInfo userInfo, Integer id) {
        Integer userId = userInfo.getId();
        Date time = new Date();
        ErpReceiptOrderDetail exit = selectOne(new EntityWrapper<ErpReceiptOrderDetail>().eq("id", id).eq("deleted", 0));
        if (ObjectUtils.isEmpty(exit)) {
            log.info("没有此详情单，调用{}的{}方法出错", "ErpReceiptOrderDetailServiceImpl", "deleteDetail()");
            throw new BusinessException(ResCodeEnum.DELETE_FAILED, ExceptionMessageEnum.ERP_RECEIPT_ORDER_DETAIL_DELETE_FAILED_NO_DATA);
        }
        Integer saleOrderId = exit.getSaleOrderId();
        BigDecimal oldMoney = exit.getReceiptAmount();
        // 查询详单对应的销售订单
        ErpSaleOrder erpSaleOrder = erpSaleOrderService.selectById(saleOrderId);
        if (ObjectUtils.isEmpty(erpSaleOrder)) {
            log.info("没有此销售订单，调用{}的{}方法出错", "ErpReceiptOrderDetailServiceImpl", "deleteDetail()");
            throw new BusinessException(ResCodeEnum.DELETE_FAILED, ExceptionMessageEnum.ERP_RECEIPT_ORDER_DETAIL_DELETE_FAILED_NO_SALE_ORDER);
        }
        // 销售订单累计收款金额 减去此详单 本次收款金额
        erpSaleOrder.setTotalReceiptAmount(erpSaleOrder.getTotalReceiptAmount().subtract(oldMoney));
        if (!erpSaleOrderService.updateById(erpSaleOrder)) {
            log.error("修改销售订单累计收款金额失败，调用{}的{}方法出错", "ErpReceiptOrderDetailServiceImpl", "deleteDetail()");
            throw new BusinessException(ResCodeEnum.DELETE_FAILED, ExceptionMessageEnum.ERP_RECEIPT_ORDER_DETAIL_DELETE_FAILED_UPDATE_SALE_ORDER_FAILED);
        }
        // 删除详情单
        if (!updateById(new ErpReceiptOrderDetail().setId(id).setDeleted(1).setModifierId(userId).setModifyTime(time))) {
            log.error("删除销售收款详情单信息失败，调用{}的{}方法出错", "ErpReceiptOrderDetailServiceImpl", "deleteDetail()");
            throw new BusinessException(ResCodeEnum.DELETE_FAILED, ExceptionMessageEnum.ERP_RECEIPT_ORDER_DETAIL_DELETE_FAILED);

        }

    }

    /**
     * 保存销售收款详情
     *
     * @param userInfo
     * @param rq
     * @return
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public Integer saveReceiptOrderDetail(AuthPlatformUserInfo userInfo, ErpReceiptOrderDetailRQ rq) {
        Date time = new Date();
        Integer orgId = userInfo.getOrgId();
        Integer userId = userInfo.getId();
        BigDecimal nowMoney = rq.getReceiptAmount();
        Integer saleOrderId = rq.getSaleOrderId();
        ErpReceiptOrderDetail order = BeanUtils.copyProperties(rq, ErpReceiptOrderDetail.class);
        order.setCreateTime(time).setCreatorId(userId).setCreatorEnterpriseId(orgId);
        ErpSaleOrder erpSaleOrder = erpSaleOrderService.selectById(saleOrderId);
        if (ObjectUtils.isEmpty(erpSaleOrder)) {
            log.info("没有此销售订单，调用{}的{}方法出错", "ErpReceiptOrderDetailServiceImpl", "saveReceiptOrderDetail()");
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_RECEIPT_ORDER_DETAIL_DELETE_FAILED_NO_SALE_ORDER);
        }
        erpSaleOrder.setTotalReceiptAmount(erpSaleOrder.getTotalReceiptAmount().add(nowMoney));
        if (!erpSaleOrderService.updateById(erpSaleOrder)) {
            log.error("修改销售订单累计收款金额失败，调用{}的{}方法出错", "ErpReceiptOrderDetailServiceImpl", "saveReceiptOrderDetail()");
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_RECEIPT_ORDER_DETAIL_DELETE_FAILED_UPDATE_SALE_ORDER_FAILED);
        }
        // 保存收款详情单
        if (!insert(order)) {
            log.error("保存收款详情单失败，调用{}的{}方法出错", "ErpReceiptOrderDetailServiceImpl", "saveReceiptOrderDetail()");
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_RECEIPT_ORDER_DETAIL_SAVE_FAILED);
        }
        return order.getId();


    }

    /**
     * 根据id查询销售收款详情
     * @param id
     * @return
     */
    @Override
    public ErpReceiptOrderDetailDTO getReceiptOrderDetailById(Integer id) {
        ErpReceiptOrderDetail detail = selectOne(new EntityWrapper<ErpReceiptOrderDetail>().eq("id",id).eq("deleted",0));
        return BeanUtils.copyProperties(detail, ErpReceiptOrderDetailDTO.class);
    }

    /**
     * @Description 根据销售订单id查询销售收款情况
     * @Param id
     * @Date 2019/6/12 19:33
     * @Author xin.huang
     * @Return
     */
    @Override
    public List<ErpReceiptOrderDetailDTO> getReceiptOrderDetailByOrderId(Integer id) {
        return receiptOrderDetailMapper.getReceiptOrderDetailByOrderId(id);
    }
}
