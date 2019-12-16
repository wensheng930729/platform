package com.bee.platform.datadriver.service.impl;

import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.entity.UserInfo;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.datadriver.dto.ErpInvoiceOrderDetailDTO;
import com.bee.platform.datadriver.entity.ErpPurchaseInvoiceOrderDetail;
import com.bee.platform.datadriver.dao.mapper.ErpPurchaseInvoiceOrderDetailMapper;
import com.bee.platform.datadriver.rq.ErpInvoiceOrderDetailRQ;
import com.bee.platform.datadriver.service.ErpPurchaseInvoiceOrderDetailService;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Date;
import java.util.List;

/**
 * <p>
 * 采购发票明细 服务实现类
 * </p>
 *
 * @author chenjie123123
 * @since 2019-06-01
 */
@Slf4j
@Service
public class ErpPurchaseInvoiceOrderDetailServiceImpl extends ServiceImpl<ErpPurchaseInvoiceOrderDetailMapper, ErpPurchaseInvoiceOrderDetail> implements ErpPurchaseInvoiceOrderDetailService {

    @Autowired
    private ErpPurchaseInvoiceOrderDetailMapper invoiceOrderDetailMapper;
    private static Integer ZERO = 0;

    /**
     * 获取采购发票明细列表
     * @param id
     * @return
     */
    @Override
    public ResponseResult<List<ErpInvoiceOrderDetailDTO>> listPurchaseInvoiceOrderDetail(String id) {
        List<ErpInvoiceOrderDetailDTO> list = invoiceOrderDetailMapper.listInvoiceOrderDetail(id);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,list);
    }

    /**
     * 删除采购发票明细信息
     * @param id
     * @return
     */
    @Override
    public ResponseResult<ResCodeEnum> deletePurchaseInvoiceOrderDetail(AuthPlatformUserInfo userInfo, String id) {
        if (invoiceOrderDetailMapper.updateById(new ErpPurchaseInvoiceOrderDetail()
                .setId(Integer.valueOf(id)).setDeleted(Status.TRUE.getKey())
                .setUpdateUser(userInfo.getId()).setUpdateTime(new Date())) <= ZERO){
            log.error("删除采购发票明细失败,调用{}类{}方法出错","ErpPurchaseInvoiceOrderDetailServiceImpl","deletePurchaseInvoiceOrderDetail()");
            throw new BusinessException(ResCodeEnum.DELETE_FAIL, ExceptionMessageEnum.ERP_PURCHASE_INVOICE_DETAIL_DELETE_FAILED);

        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * 保存采购发票明细发票
     * @param userInfo
     * @param rq
     * @return
     */
    @Override
    public ResponseResult<ResCodeEnum> savePurchaseInvoiceOrderDetail(AuthPlatformUserInfo userInfo, ErpInvoiceOrderDetailRQ rq) {
        ErpPurchaseInvoiceOrderDetail invoiceOrder = BeanUtils.copyProperties(rq,ErpPurchaseInvoiceOrderDetail.class);
        // 新增
        if (invoiceOrder.getId() == null){
            if (invoiceOrderDetailMapper.insert(invoiceOrder
                    .setDeleted(Status.FALSE.getKey())
                    .setCreateUser(userInfo.getId())
                    .setCreateTime(new Date())) <= ZERO){
                log.error("新增采购发票明细失败,调用{}类{}方法出错","ErpPurchaseInvoiceOrderDetailServiceImpl","savePurchaseInvoiceOrderDetail()");
                throw new BusinessException(ResCodeEnum.DELETE_FAIL, ExceptionMessageEnum.ERP_PURCHASE_INVOICE_DETAIL_ADD_FAILED);
            }
            // 修改
        }else {
            if (invoiceOrderDetailMapper.updateById(invoiceOrder
                    .setId(rq.getId())
                    .setUpdateUser(userInfo.getId())
                    .setUpdateTime(new Date())) <= ZERO){
                log.error("修改采购发票明细失败,调用{}类{}方法出错","ErpPurchaseInvoiceOrderDetailServiceImpl","savePurchaseInvoiceOrderDetail()");
                throw new BusinessException(ResCodeEnum.DELETE_FAIL, ExceptionMessageEnum.ERP_PURCHASE_INVOICE_DETAIL_UPDATE_FAILED);
            }

        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }
}
