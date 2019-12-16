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
import com.bee.platform.datadriver.entity.ErpSaleInvoiceOrderDetail;
import com.bee.platform.datadriver.dao.mapper.ErpSaleInvoiceOrderDetailMapper;
import com.bee.platform.datadriver.rq.ErpInvoiceOrderDetailRQ;
import com.bee.platform.datadriver.service.ErpSaleInvoiceOrderDetailService;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Date;
import java.util.List;

/**
 * <p>
 * 销售发票明细 服务实现类
 * </p>
 *
 * @author chenjie123123
 * @since 2019-06-01
 */
@Slf4j
@Service
public class ErpSaleInvoiceOrderDetailServiceImpl extends ServiceImpl<ErpSaleInvoiceOrderDetailMapper, ErpSaleInvoiceOrderDetail> implements ErpSaleInvoiceOrderDetailService {

    @Autowired
    private ErpSaleInvoiceOrderDetailMapper invoiceOrderDetailMapper;
    private static Integer ZERO = 0;

    /**
     * 获取销售发票明细信息
     * @param id
     * @return
     */
    @Override
    public ResponseResult<List<ErpInvoiceOrderDetailDTO>> listSaleInvoiceOrderDetail(String id) {
        List<ErpInvoiceOrderDetailDTO> list = invoiceOrderDetailMapper.listInvoiceOrderDetail(id);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,list);
    }

    /**
     * 删除销售发票明细信息
     * @param id
     * @return
     */
    @Override
    public ResponseResult<ResCodeEnum> deleteSaleInvoiceOrderDetail(AuthPlatformUserInfo userInfo, String id) {
        if (invoiceOrderDetailMapper.updateById(new ErpSaleInvoiceOrderDetail()
                .setId(Integer.valueOf(id)).setDeleted(Status.TRUE.getKey())
                .setUpdateUser(userInfo.getId()).setUpdateTime(new Date())) <= ZERO){
            log.error("删除采购发票明细失败,调用{}类{}方法出错","ErpSaleInvoiceOrderDetailServiceImpl","deleteSaleInvoiceOrderDetail()");
            throw new BusinessException(ResCodeEnum.DELETE_FAIL, ExceptionMessageEnum.ERP_SALE_INVOICE_DETAIL_DELETE_FAILED);

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
    public ResponseResult<ResCodeEnum> saveSaleInvoiceOrderDetail(AuthPlatformUserInfo userInfo, ErpInvoiceOrderDetailRQ rq) {
        ErpSaleInvoiceOrderDetail invoiceOrder = BeanUtils.copyProperties(rq,ErpSaleInvoiceOrderDetail.class);
        // 新增
        if (invoiceOrder.getId() == null){
            if (invoiceOrderDetailMapper.insert(invoiceOrder
                    .setDeleted(Status.FALSE.getKey())
                    .setCreateUser(userInfo.getId())
                    .setCreateTime(new Date())) <= ZERO){
                log.error("新增销售发票明细失败,调用{}类{}方法出错","ErpSaleInvoiceOrderDetailServiceImpl","saveSaleInvoiceOrderDetail()");
                throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_SALE_INVOICE_DETAIL_ADD_FAILED);
            }
            // 修改
        }else {
            if (invoiceOrderDetailMapper.updateById(invoiceOrder
                    .setId(rq.getId())
                    .setUpdateUser(userInfo.getId())
                    .setUpdateTime(new Date())) <= ZERO){
                log.error("修改销售发票明细失败,调用{}类{}方法出错","ErpSaleInvoiceOrderDetailServiceImpl","saveSaleInvoiceOrderDetail()");
                throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_SALE_INVOICE_DETAIL_UPDATE_FAILED);
            }

        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }
}
