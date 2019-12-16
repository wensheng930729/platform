package com.bee.platform.datadriver.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.entity.UserInfo;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.datadriver.dto.ErpPurchaseOrderDetailDTO;
import com.bee.platform.datadriver.entity.ErpPurchaseOrderDetail;
import com.bee.platform.datadriver.dao.mapper.ErpPurchaseOrderDetailMapper;
import com.bee.platform.datadriver.rq.PurchaseOrderDetailSaveRQ;
import com.bee.platform.datadriver.service.ErpPurchaseOrderDetailService;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.util.Date;
import java.util.List;

/**
 * <p>
 * 采购单明细 服务实现类
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
@Slf4j
@Service
public class ErpPurchaseOrderDetailServiceImpl extends ServiceImpl<ErpPurchaseOrderDetailMapper, ErpPurchaseOrderDetail> implements ErpPurchaseOrderDetailService {

    @Autowired
    private ErpPurchaseOrderDetailMapper purchaseOrderDetailMapper;
    private static Integer ZERO = 0;

    /**
     * 分页查询采购订单明细列表
     * @param pagination
     * @param id
     * @return
     */
    @Override
    public List<ErpPurchaseOrderDetailDTO> listErpPurchaseOrderDetail(Pagination pagination, String id) {
        /*return purchaseOrderDetailMapper.selectPage(pagination,new EntityWrapper<ErpPurchaseOrderDetail>()
                .eq("order_id",id).and()
                .eq("deleted", Status.FALSE.getKey()));*/
        List<ErpPurchaseOrderDetailDTO> list = purchaseOrderDetailMapper.selectPurchaseOrderDetail(pagination,id);
        return list;
    }

    /**
     * 删除采购订单明细
     * @param userInfo
     * @param id
     * @return
     */
    @Override
    public ResponseResult<ResCodeEnum> deleteErpPurchaseOrderDetail(AuthPlatformUserInfo userInfo, String id) {
        if (purchaseOrderDetailMapper.updateById(new ErpPurchaseOrderDetail()
                .setId(Integer.valueOf(id)).setDeleted(Status.TRUE.getKey())) <= ZERO){
            log.error("删除采购单明细失败,调用{}类{}方法出错","ErpPurchaseOrderDetailServiceImpl","deleteErpPurchaseOrderDetail()");
            throw new BusinessException(ResCodeEnum.DELETE_FAIL, ExceptionMessageEnum.ERP_PURCHASE_DETAIL_DELETE_FAILED);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * 保存采购订单明细
     * @param userInfo
     * @param rq
     * @return
     */
    @Override
    public ResponseResult<ResCodeEnum> saveErpPurchaseOrderDetail(AuthPlatformUserInfo userInfo, PurchaseOrderDetailSaveRQ rq) {
        ErpPurchaseOrderDetail purchaseOrderDetail = BeanUtils.copyProperties(rq,ErpPurchaseOrderDetail.class);
        List<ErpPurchaseOrderDetail> check = purchaseOrderDetailMapper.selectList(new EntityWrapper<ErpPurchaseOrderDetail>(new ErpPurchaseOrderDetail()
                .setOrderId(rq.getOrderId()).setDeleted(Status.FALSE.getKey())));
        if (!CollectionUtils.isEmpty(check)){
            if (!rq.getProductId().equals(check.get(0).getProductId())){
                throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_PURCHASE_PRODUCT_REPEAT);
            }
        }
        // 新增
        if (purchaseOrderDetail.getId() == null){
            if (purchaseOrderDetailMapper.insert(purchaseOrderDetail
                    .setDeleted(Status.FALSE.getKey())
                    .setCreateUser(userInfo.getId())
                    .setCreateTime(new Date())) <= ZERO){
                log.error("新增采购单明细失败,调用{}类{}方法出错","ErpPurchaseOrderDetailServiceImpl","saveErpPurchaseOrderDetail()");
                throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_PURCHASE_DETAIL_ADD_FAILED);
            }
        // 修改
        }else {
            if (purchaseOrderDetailMapper.updateById(purchaseOrderDetail
                    .setId(rq.getId())
                    .setDeleted(Status.FALSE.getKey())
                    .setCreateUser(userInfo.getId())
                    .setCreateTime(new Date())) <= ZERO){
                log.error("修改采购单明细失败,调用{}类{}方法出错","ErpPurchaseOrderDetailServiceImpl","saveErpPurchaseOrderDetail()");
                throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_PURCHASE_DETAIL_UPDATE_FAILED);
            }

        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }
}
