package com.bee.platform.datadriver.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.baomidou.mybatisplus.toolkit.CollectionUtils;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.datadriver.dao.mapper.*;
import com.bee.platform.datadriver.dto.ErpProductCategoryNameDTO;
import com.bee.platform.datadriver.dto.ErpRepoReceiptDetailProductOutDTO;
import com.bee.platform.datadriver.dto.ErpRepoReceiptDetailRawInDTO;
import com.bee.platform.datadriver.dto.RepoReceiptDetailDTO;
import com.bee.platform.datadriver.entity.*;
import com.bee.platform.datadriver.enums.EnumBusinessType;
import com.bee.platform.datadriver.enums.EnumErpSaleOrderStatus;
import com.bee.platform.datadriver.enums.EnumSaleOrderStatus;
import com.bee.platform.datadriver.rq.ReceiptDetailRQ;
import com.bee.platform.datadriver.rq.RepoReceiptDetailProductOutRQ;
import com.bee.platform.datadriver.rq.RepoReceiptDetailRQ;
import com.bee.platform.datadriver.rq.RepoReceiptDetailRawInRQ;
import com.bee.platform.datadriver.service.*;
import io.swagger.models.auth.In;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ObjectUtils;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * <p>
 * 仓库单明细 服务实现类
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
@Slf4j
@Service
public class ErpRepoReceiptDetailServiceImpl extends ServiceImpl<ErpRepoReceiptDetailMapper, ErpRepoReceiptDetail> implements ErpRepoReceiptDetailService {

    @Autowired
    private ErpRepoReceiptDetailMapper repoReceiptDetailMapper;
    @Autowired
    private ErpPurchaseOrderMapper purchaseOrderMapper;
    @Autowired
    private ErpRepositoryReceiptMapper repositoryReceiptMapper;

    @Autowired
    private ErpRepositoryReceiptService erpRepositoryReceiptService;

    @Autowired
    private ErpStockService erpStockService;

    @Autowired
    private ErpProductService erpProductService;


    @Autowired
    private ErpPurchaseOrderService erpPurchaseOrderService;

    @Autowired
    private ErpSaleOrderService erpSaleOrderService;

    @Autowired
    private ErpRepositoryService erpRepositoryService;


    @Autowired
    private CommonMapper commonMapper;

    @Autowired
    private ErpProductService productService;

    @Autowired
    private ErpSaleOrderDetailMapper erpSaleOrderDetailMapper;

    @Autowired
    private ErpPurchaseOrderDetailMapper purchaseOrderDetailMapper;

    @Autowired
    private ErpSaleStatementMapper saleStatementMapper;

    @Autowired
    private ErpSaleInvoiceOrderMapper saleInvoiceOrderMapper;

    /**
     *
     * @param rq
     * @return
     */
    @Override
    public List<RepoReceiptDetailDTO> listRepoReceiptDetail(ReceiptDetailRQ rq) {
        List<RepoReceiptDetailDTO> repoReceiptDetail = new ArrayList<>();
        if (EnumBusinessType.MATERIAL_STOCK.getCode().equals(rq.getBusinessType())){
            repoReceiptDetail = repoReceiptDetailMapper.selectByOrderId(rq.getOrderId(),rq.getBusinessType());
        }
        if (EnumBusinessType.PRODUCT_DELIVERY.getCode().equals(rq.getBusinessType())){
            repoReceiptDetail = repoReceiptDetailMapper.selectByOrderId(rq.getOrderId(),rq.getBusinessType());
        }
        return repoReceiptDetail;
    }

    /**
     * @Description 新增采购收货明细
     * @Param simpleUserInfo
     * @Param repoReceiptDetailRQ
     * @Date 2019/6/4 15:45
     * @Author xin.huang
     * @Return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<Integer> addPurchaseGoodsOrderDetail(AuthPlatformUserInfo simpleUserInfo, RepoReceiptDetailRQ repoReceiptDetailRQ) {
        ErpRepoReceiptDetail repoReceiptDetail = BeanUtils.copyProperties(repoReceiptDetailRQ, ErpRepoReceiptDetail.class);
        repoReceiptDetail.setCreateUser(simpleUserInfo.getId());
        repoReceiptDetailMapper.insert(repoReceiptDetail);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, repoReceiptDetail.getId());
    }

    /**
     * @Description 更新采购收货明细
     * @Param id
     * @Param repoReceiptDetailRQ
     * @Date 2019/6/4 15:45
     * @Author xin.huang
     * @Return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<Integer> updatePurchaseGoodsOrderDetail(RepoReceiptDetailRQ repoReceiptDetailRQ) {
        if (Objects.isNull(repoReceiptDetailRQ.getId())) {
            return ResponseResult.buildResponseResult(ResCodeEnum.PARAMETER_INCOMPLETE);
        }
        ErpRepoReceiptDetail repoReceiptDetail = repoReceiptDetailMapper.selectById(repoReceiptDetailRQ.getId());
        if (Objects.isNull(repoReceiptDetail)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NO_DATA);
        }
        ErpRepoReceiptDetail receiptDetail = BeanUtils.copyProperties(repoReceiptDetailRQ, ErpRepoReceiptDetail.class);
        repoReceiptDetailMapper.updateById(receiptDetail);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, repoReceiptDetailRQ.getId());
    }

    /**
     * @Description 删除采购收货明细
     * @Param id
     * @Date 2019/6/4 15:45
     * @Author xin.huang
     * @Return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> deletePurchaseGoodsOrderDetail(Integer id) {
        if (Objects.isNull(id)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.PARAMETER_INCOMPLETE);
        }
        repoReceiptDetailMapper.updateById(new ErpRepoReceiptDetail().setId(id).setDeleted(Status.TRUE.getKey()));
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * @Description 查询采购收货明细
     * @Param id
     * @Date 2019/6/4 15:45
     * @Author xin.huang
     * @Return
     */
    @Override
    public ResponseResult<List<RepoReceiptDetailDTO>> findReoReceiptDetailInfo(Integer id) {
//        ErpRepositoryReceipt repositoryReceipt = repositoryReceiptMapper
//                .selectOne(new ErpRepositoryReceipt().setId(id).setDeleted(Status.FALSE.getKey()));
        List<RepoReceiptDetailDTO> repoReceiptDetails = repoReceiptDetailMapper.findReoReceiptDetailInfo(id);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, repoReceiptDetails);
    }


    /**
     * 保存原料入库明细单
     * @param userInfo
     * @param rq
     * @return
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public Integer saveRepoReceiptDetailRawIn(AuthPlatformUserInfo userInfo, RepoReceiptDetailRawInRQ rq) {

        Integer userId = userInfo.getId();
        Integer rqId = rq.getId();
        ErpRepoReceiptDetail order = BeanUtils.copyProperties(rq, ErpRepoReceiptDetail.class);
        if(ObjectUtils.isEmpty(rqId)){
            order.setCreateUser(userId);
        }
        Integer receiptId = rq.getReceiptId();
        if(ObjectUtils.isEmpty(receiptId)){
            log.error("保存原料入库明细失败，没有关联的的原料入库主订单id 调用{}的{}方法出错", "ErpRepoReceiptDetailServiceImpl", "saveRepoReceiptDetailRawIn()");
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_REPOSITORY_RECEIPT_RAW_IN_DETAIL_SAVE_FAILED_NO_MAIN_ID);
        }

        ErpRepositoryReceipt re = erpRepositoryReceiptService.selectById(receiptId);
        if(ObjectUtils.isEmpty(re)||ObjectUtils.isEmpty(re.getOrgId())){
            log.error("保存原料入库明细失败，没有找到关联的的原料入库主订单信息 调用{}的{}方法出错", "ErpRepoReceiptDetailServiceImpl", "saveRepoReceiptDetailRawIn()");
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_REPOSITORY_RECEIPT_RAW_IN_DETAIL_SAVE_FAILED_NO_MAIN_ORDER);
        }
        Integer relatedOrderId = re.getRelatedOrderId();
        // 查询源采购订单
        ErpPurchaseOrder erpPurchaseOrder = erpPurchaseOrderService.selectById(relatedOrderId);
        if(ObjectUtils.isEmpty(erpPurchaseOrder)){
            log.error("保存原料入库明细失败，没有查到相关的源采购订单信息 调用{}的{}方法出错", "ErpRepoReceiptDetailServiceImpl", "saveRepoReceiptDetailRawIn()");
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_REPOSITORY_RECEIPT_RAW_IN_DETAIL_SAVE_FAILED_NO_SALE_ORDER);
        }
//        Integer companyName = erpPurchaseOrder.getCompany();
//        if(ObjectUtils.isEmpty(companyName)){
//            log.error("保存原料入库明细失败，没有查到相关的源采购订单中 的公司信息 调用{}的{}方法出错", "ErpRepoReceiptDetailServiceImpl", "saveRepoReceiptDetailRawIn()");
//            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_REPOSITORY_RECEIPT_RAW_IN_DETAIL_SAVE_FAILED);
//        }

        Integer companyId = re.getOrgId();
        String companyName = commonMapper.getCompanyNameById(companyId);
        Integer productId = rq.getProductId();
        Integer repositoryId = rq.getRepositoryId();
        ErpRepository erpRepository = erpRepositoryService.selectById(repositoryId);
        String repositoryName ="";
        if(!ObjectUtils.isEmpty(erpRepository)){
             repositoryName = erpRepository.getName();
        }

        String unit = rq.getUnit();
        BigDecimal inNum = rq.getNum();
        ErpProductCategoryNameDTO category = erpProductService.getProductCategoryName(productId);
        String categoryName= "";
        String productName="";
        if(!ObjectUtils.isEmpty(category)){
            categoryName = category.getCategoryName();
            productName = category.getProductName();
        }

        ErpStock existStock = erpStockService.selectOne(new EntityWrapper<ErpStock>().eq("org_id",companyId).eq("product_id", productId).eq("repository_id", repositoryId).eq("deleted", 0));
        // 如果不存在则创建新记录
        if(ObjectUtils.isEmpty(existStock)){
            ErpStock stock =   new ErpStock()
                    .setOrgId(companyId)
                    .setCompanyName(companyName)
                    .setRepositoryId(repositoryId)
                    .setProductId(productId)
                    .setProductName(productName)
                    .setProductCategory(categoryName)
                    .setUnit(unit)
                    .setRepositoryName(repositoryName)
                    .setInStockNum(inNum)
                    .setStockNum(inNum)
                    ;
            if(!erpStockService.insert(stock)){
                log.error("保存原料入库明细失败,保存库存信息失败，调用{}的{}方法出错", "ErpRepoReceiptDetailServiceImpl", "saveRepoReceiptDetailRawIn()");
                throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_REPOSITORY_RECEIPT_RAW_IN_DETAIL_SAVE_FAILED_UPDATE_STOCK_FAILED);

            }
            // 如果存在记录则修改
        }else {

            // 如果是新添加的
            if(ObjectUtils.isEmpty(rqId)){
                BigDecimal addInStock = inNum.add(existStock.getInStockNum());
                BigDecimal addStock = inNum.add(existStock.getStockNum());
                existStock.setInStockNum(addInStock);
                existStock.setStockNum(addStock);
            }else {
                // 如果是编辑
                ErpRepoReceiptDetail exist = selectById(rqId);
                if(!ObjectUtils.isEmpty(exist)){
                    BigDecimal oldInNum = exist.getNum();
                    BigDecimal oldInStockNum = existStock.getInStockNum();
                    BigDecimal oldStockNum = existStock.getStockNum();
                    existStock.setInStockNum(oldInStockNum.add(inNum).subtract(oldInNum));
                    existStock.setStockNum(oldStockNum.add(inNum).subtract(oldInNum));

                }else {

                    log.info("保存原料入库明细时，调整库存，找不到相关明细记录");
                }

            }

            if(!erpStockService.updateById(existStock)){
                log.error("保存原料入库明细失败,保存库存信息失败，调用{}的{}方法出错", "ErpRepoReceiptDetailServiceImpl", "saveRepoReceiptDetailRawIn()");
                throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_REPOSITORY_RECEIPT_RAW_IN_DETAIL_SAVE_FAILED_UPDATE_STOCK_FAILED);
            }
        }
        if(!insertOrUpdate(order)){
            log.error("保存原料入库明细失败，调用{}的{}方法出错", "ErpRepoReceiptDetailServiceImpl", "saveRepoReceiptDetailRawIn()");
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_REPOSITORY_RECEIPT_RAW_IN_DETAIL_SAVE_FAILED);
        }
        this.updateOrderReceiveStatu(order);
        return order.getId();
    }

    /**
     * 修改订单收货状态
     * @param order
     */
    private void updateOrderReceiveStatu(ErpRepoReceiptDetail order) {
        ErpRepositoryReceipt repositoryReceipt = repositoryReceiptMapper.selectById(order.getReceiptId());
        // 待收货
        ErpPurchaseOrder purchaseOrder = purchaseOrderMapper.selectById(repositoryReceipt.getRelatedOrderId());
        List<ErpPurchaseOrderDetail> purchaseOrderDetails = purchaseOrderDetailMapper.selectList(new EntityWrapper<ErpPurchaseOrderDetail>()
                .eq("deleted",Status.FALSE.getKey()).and()
                .eq("order_id",purchaseOrder.getId()));
        BigDecimal sum = BigDecimal.ZERO;
        if (!CollectionUtils.isEmpty(purchaseOrderDetails)){
            sum = purchaseOrderDetails.stream().map(o->o.getNum()).reduce(BigDecimal::add).orElse(BigDecimal.ZERO);
        }
        BigDecimal receive = BigDecimal.ZERO;
        List<ErpRepositoryReceipt> receipts = repositoryReceiptMapper.selectList(new EntityWrapper<ErpRepositoryReceipt>()
                .eq("related_order_id",purchaseOrder.getId()).and()
                .eq("deleted",Status.FALSE.getKey()).and()
                .eq("type",EnumBusinessType.MATERIAL_STOCK.getCode()));
        if (!CollectionUtils.isEmpty(receipts)){
            List<Integer> rids = receipts.stream().map(o -> o.getId()).collect(Collectors.toList());
            List<ErpRepoReceiptDetail> details = repoReceiptDetailMapper.selectList(new EntityWrapper<ErpRepoReceiptDetail>()
                    .eq("deleted",Status.FALSE.getKey())
                    .in("receipt_id",rids));
            if (!CollectionUtils.isEmpty(details)){
                receive = details.stream().map(o->o.getNum()).reduce(BigDecimal::add).orElse(BigDecimal.ZERO);
            }
        }
        if (sum.compareTo(BigDecimal.ZERO)>0){
            if (receive.compareTo(BigDecimal.ZERO) == 0){
                if (purchaseOrderMapper.updateById(new ErpPurchaseOrder()
                        .setId(repositoryReceipt.getRelatedOrderId()).setReceiveState(Status.FALSE.getKey())) < 0){
                    log.error("修改订单收货状态失败，调用{}的{}方法出错", "ErpRepoReceiptDetailServiceImpl", "updateOrderReceiveStatu()");
                    throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_REPOSITORY_RECEIPT_RAW_IN_DETAIL_SAVE_FAILED);

                }
            }else if (receive.divide(sum, 2, BigDecimal.ROUND_HALF_UP).compareTo(new BigDecimal("0.95")) <= 0){
                if (purchaseOrderMapper.updateById(new ErpPurchaseOrder()
                        .setId(repositoryReceipt.getRelatedOrderId()).setReceiveState(Status.TRUE.getKey())) < 0){
                    log.error("修改订单收货状态失败，调用{}的{}方法出错", "ErpRepoReceiptDetailServiceImpl", "updateOrderReceiveStatu()");
                    throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_REPOSITORY_RECEIPT_RAW_IN_DETAIL_SAVE_FAILED);

                }
            }else {
                if (purchaseOrderMapper.updateById(new ErpPurchaseOrder()
                        .setId(repositoryReceipt.getRelatedOrderId()).setReceiveState(2)) < 0){
                    log.error("修改订单收货状态失败，调用{}的{}方法出错", "ErpRepoReceiptDetailServiceImpl", "updateOrderReceiveStatu()");
                    throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_REPOSITORY_RECEIPT_RAW_IN_DETAIL_SAVE_FAILED);

                }
            }
        }
    }

    /**
     * 根据id删除原料入库明细
     * @param userInfo
     * @param id
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public void deleteRepoReceiptDetailRawIn(AuthPlatformUserInfo userInfo, Integer id) {

        ErpRepoReceiptDetail exist = selectOne(new EntityWrapper<ErpRepoReceiptDetail>().eq("id", id).eq("deleted", 0));
        if(ObjectUtils.isEmpty(exist)){
            log.info("没有找到相关原料入库明细详情");
            return;
        }
        BigDecimal oldInNum = exist.getNum();
        Integer productId = exist.getProductId();
        Integer repositoryId = exist.getRepositoryId();

        Integer receiptId = exist.getReceiptId();
        if(ObjectUtils.isEmpty(receiptId)){
            log.error("删除原料入库明细失败，没有关联的的原料入库主订单id 调用{}的{}方法出错", "ErpRepoReceiptDetailServiceImpl", "deleteRepoReceiptDetailRawIn()");
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_REPOSITORY_RECEIPT_RAW_IN_DETAIL_SAVE_FAILED_NO_MAIN_ID);
        }

        ErpRepositoryReceipt re = erpRepositoryReceiptService.selectById(receiptId);
        if(ObjectUtils.isEmpty(re)||ObjectUtils.isEmpty(re.getOrgId())){
            log.error("删除原料入库明细失败，没有找到关联的的原料入库主订单信息 调用{}的{}方法出错", "ErpRepoReceiptDetailServiceImpl", "deleteRepoReceiptDetailRawIn()");
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_REPOSITORY_RECEIPT_RAW_IN_DETAIL_SAVE_FAILED_NO_MAIN_ORDER);
        }
        Integer companyId = re.getOrgId();
        Integer relatedOrderId = re.getRelatedOrderId();
        // 查询源采购订单
        ErpPurchaseOrder erpPurchaseOrder = erpPurchaseOrderService.selectById(relatedOrderId);
        if(ObjectUtils.isEmpty(erpPurchaseOrder)){
            log.error("删除原料入库明细失败，没有查到相关的源采购订单信息 调用{}的{}方法出错", "ErpRepoReceiptDetailServiceImpl", "deleteRepoReceiptDetailRawIn()");
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_REPOSITORY_RECEIPT_RAW_IN_DETAIL_DELETE_FAILED);
        }
//        String companyName = erpPurchaseOrder.getCompanyName();
//        if(ObjectUtils.isEmpty(companyName)){
//            log.error("删除原料入库明细失败，没有查到相关的源采购订单中 的公司信息 调用{}的{}方法出错", "ErpRepoReceiptDetailServiceImpl", "deleteRepoReceiptDetailRawIn()");
//            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_REPOSITORY_RECEIPT_RAW_IN_DETAIL_DELETE_FAILED);
//        }

        // 查询库存记录
        ErpStock existStock = erpStockService.selectOne(new EntityWrapper<ErpStock>().eq("org_id",companyId).eq("product_id", productId).eq("repository_id", repositoryId).eq("deleted", 0));
        if(!ObjectUtils.isEmpty(existStock)){
            // 有库存记录 修改记录
            BigDecimal oldInStockNum = existStock.getInStockNum();
            BigDecimal oldStockNum = existStock.getStockNum();
            // 删除 减去old
            existStock.setInStockNum(oldInStockNum.subtract(oldInNum));
            // 现有量 减去 old
            existStock.setStockNum(oldStockNum.subtract(oldInNum));

            if(!erpStockService.updateById(existStock)){
                log.error("删除原料入库明细失败，调整库存数量失败,调用{}的{}方法出错", "ErpRepoReceiptDetailServiceImpl", "deleteRepoReceiptDetailRawIn()");
                throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.ERP_REPOSITORY_RECEIPT_RAW_IN_DETAIL_DELETE_FAILED_UPDATE_STOCK_FAILED);

            }
        }else {

            log.info("删除原料入库明细时，调整库存量，没有找到相关记录");
        }

        if(!updateById(new ErpRepoReceiptDetail().setId(id).setDeleted(1))){
            log.error("删除原料入库明细失败，调用{}的{}方法出错", "ErpRepoReceiptDetailServiceImpl", "deleteRepoReceiptDetailRawIn()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.ERP_REPOSITORY_RECEIPT_RAW_IN_DETAIL_DELETE_FAILED);
        }
        this.updateOrderReceiveStatu(exist);

    }

    /**
     * 根据主单id 查询详情列表
     * @param receiptId
     * @return
     */
    @Override
    public List<ErpRepoReceiptDetailRawInDTO> getRepoReceiptDetailRawInList(Integer receiptId) {

        return baseMapper.getRepoReceiptDetailRawInList(receiptId);
    }


    /**
     * 保存成品出库明细
     * @param userInfo
     * @param rq
     * @return
     */

    @Transactional(rollbackFor = Exception.class)
    @Override
    public Integer saveRepoReceiptDetailProductOut(AuthPlatformUserInfo userInfo, RepoReceiptDetailProductOutRQ rq) {

        Integer userId = userInfo.getId();
        Integer rqId = rq.getId();
        ErpRepoReceiptDetail order = BeanUtils.copyProperties(rq, ErpRepoReceiptDetail.class);
        if(ObjectUtils.isEmpty(rqId)){
            order.setCreateUser(userId);
        }
        Integer receiptId = rq.getReceiptId();
        if(ObjectUtils.isEmpty(receiptId)){
            log.error("保存成品出库明细失败，没有关联的的成品出库主订单id 调用{}的{}方法出错", "ErpRepoReceiptDetailServiceImpl", "saveRepoReceiptDetailProductOut()");
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_REPOSITORY_RECEIPT_PRODUCT_OUT_DETAIL_SAVE_FAILED_NO_MAIN_ID);
        }

        ErpRepositoryReceipt re = erpRepositoryReceiptService.selectById(receiptId);
        if(ObjectUtils.isEmpty(re)||ObjectUtils.isEmpty(re.getOrgId())){
            log.error("保存成品出库明细失败，没有找到关联的的成品出库主订单信息 调用{}的{}方法出错", "ErpRepoReceiptDetailServiceImpl", "saveRepoReceiptDetailProductOut()");
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_REPOSITORY_RECEIPT_PRODUCT_OUT_DETAIL_SAVE_FAILED_NO_MAIN_ORDER);
        }
        Integer relatedOrderId = re.getRelatedOrderId();
        // 查询源销售订单
        ErpSaleOrder erpSaleOrder = erpSaleOrderService.selectById(relatedOrderId);
        if(ObjectUtils.isEmpty(erpSaleOrder)){
            log.error("保存成品出库明细失败，没有查到相关的源销售订单信息 调用{}的{}方法出错", "ErpRepoReceiptDetailServiceImpl", "saveRepoReceiptDetailProductOut()");
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_REPOSITORY_RECEIPT_PRODUCT_OUT_DETAIL_SAVE_FAILED_NO_SALE_ORDER);
        }
//        String companyName = erpSaleOrder.getCompanyName();
//        if(ObjectUtils.isEmpty(companyName)){
//            log.error("保存成品出库明细失败，没有查到相关的源销售订单中 的公司信息 调用{}的{}方法出错", "ErpRepoReceiptDetailServiceImpl", "saveRepoReceiptDetailProductOut()");
//            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_REPOSITORY_RECEIPT_PRODUCT_OUT_DETAIL_SAVE_FAILED);
//        }

        Integer companyId = re.getOrgId();
        Integer productId = rq.getProductId();
        Integer repositoryId = rq.getRepositoryId();
        BigDecimal outNum = rq.getNum();

        ErpStock existStock = erpStockService.selectOne(new EntityWrapper<ErpStock>().eq("org_id",companyId).eq("product_id", productId).eq("repository_id", repositoryId).eq("deleted", 0));
        if(ObjectUtils.isEmpty(existStock)){
            log.error("保存成品出库明细失败,库存表里没有该产品，不能出库不存在的产品，调用{}的{}方法出错", "ErpRepoReceiptDetailServiceImpl", "saveRepoReceiptDetailProductOut()");
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_REPOSITORY_RECEIPT_PRODUCT_OUT_DETAIL_SAVE_FAILED_NO_STOCK);
        }else {

            // 修改出库数量
            BigDecimal outStockNum = existStock.getOutStockNum();
            BigDecimal stockNum = existStock.getStockNum();
            // 新增
            if(ObjectUtils.isEmpty(rqId)){

                // 如果出库数量大于 现有量 则不能出库 抛异常
                // 如果出库数量大于 现有量 则不能出库 抛异常
                if(outNum.compareTo(stockNum)>0){
                    log.error("保存成品出库明细失败,库存现有量小于出库数量，不能超额出库产品，调用{}的{}方法出错", "ErpRepoReceiptDetailServiceImpl", "saveRepoReceiptDetailProductOut()");
                    throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_REPOSITORY_RECEIPT_PRODUCT_OUT_DETAIL_SAVE_FAILED_NO_STOCK_NUM);

                }
                existStock.setOutStockNum(outNum.add(outStockNum));
                existStock.setStockNum(stockNum.subtract(outNum));
            }else {
                ErpRepoReceiptDetail exist = selectById(rqId);
                if(!ObjectUtils.isEmpty(exist)){
                    // 编辑
                    BigDecimal oldNumber = exist.getNum();
                    // 新的减旧的 差值
                    BigDecimal newNumber = outNum.subtract(oldNumber);
                    // 出库数量 加上差值
                    existStock.setOutStockNum(newNumber.add(outStockNum));
                    if(newNumber.compareTo(stockNum)>0){
                        log.error("保存成品出库明细失败,库存现有量小于出库数量，不能超额出库产品，调用{}的{}方法出错", "ErpRepoReceiptDetailServiceImpl", "saveRepoReceiptDetailProductOut()");
                        throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_REPOSITORY_RECEIPT_PRODUCT_OUT_DETAIL_SAVE_FAILED_NO_STOCK_NUM);

                    }
                    // 现存量 减去 差值
                    existStock.setStockNum(stockNum.subtract(newNumber));
                }else {

                    log.info("保存成品出库明细时，调整库存，找不到相关明细记录");
                }

            }

        }
        if(!erpStockService.updateById(existStock)){
            log.error("保存成品出库明细失败,保存库存信息失败，调用{}的{}方法出错", "ErpRepoReceiptDetailServiceImpl", "saveRepoReceiptDetailProductOut()");
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_REPOSITORY_RECEIPT_PRODUCT_OUT_DETAIL_SAVE_FAILED_UPDATE_STOCK_FAILED);
        }

        if(!insertOrUpdate(order)){
            log.error("保存成品出库明细失败，调用{}的{}方法出错", "ErpRepoReceiptDetailServiceImpl", "saveRepoReceiptDetailProductOut()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.ERP_REPOSITORY_RECEIPT_PRODUCT_OUT_DETAIL_SAVE_FAILED);
        }

        //更新订单状态
        updateSaleOrderStatus(relatedOrderId);

        return order.getId();
    }

    /**
     * 根据id删除成品出库明细
     * @param userInfo
     * @param id
     */

    @Transactional(rollbackFor = Exception.class)
    @Override
    public void deleteRepoReceiptDetailProductOutById(AuthPlatformUserInfo userInfo, Integer id) {

        ErpRepoReceiptDetail exist = selectOne(new EntityWrapper<ErpRepoReceiptDetail>().eq("id", id).eq("deleted", 0));
        if(ObjectUtils.isEmpty(exist)){
            log.info("没有找到相关成品出库明细详情");
            return;
        }
        BigDecimal oldOutNum = exist.getNum();
        Integer productId = exist.getProductId();
        Integer repositoryId = exist.getRepositoryId();

        Integer receiptId = exist.getReceiptId();
        if(ObjectUtils.isEmpty(receiptId)){
            log.error("删除成品出库明细失败，没有关联的的成品出库主订单id 调用{}的{}方法出错", "ErpRepoReceiptDetailServiceImpl", "deleteRepoReceiptDetailProductOutById()");
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_REPOSITORY_RECEIPT_PRODUCT_OUT_DETAIL_DELETE_FAILED_NO_MAIN_ID);
        }

        ErpRepositoryReceipt re = erpRepositoryReceiptService.selectById(receiptId);
        if(ObjectUtils.isEmpty(re)||ObjectUtils.isEmpty(re.getOrgId())){
            log.error("删除成品出库明细失败，没有找到关联的的成品出库主订单信息 调用{}的{}方法出错", "ErpRepoReceiptDetailServiceImpl", "deleteRepoReceiptDetailProductOutById()");
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_REPOSITORY_RECEIPT_PRODUCT_OUT_DETAIL_DELETE_FAILED_NO_MAIN_ORDER);
        }
        Integer companyId = re.getOrgId();

        // 查询库存记录
        ErpStock existStock = erpStockService.selectOne(new EntityWrapper<ErpStock>().eq("org_id",companyId).eq("product_id", productId).eq("repository_id", repositoryId).eq("deleted", 0));
        if(!ObjectUtils.isEmpty(existStock)){
            // 有库存记录 修改记录
            BigDecimal oldOutStockNum = existStock.getOutStockNum();
            BigDecimal oldStockNum = existStock.getStockNum();
            // 删除 加上old
            existStock.setOutStockNum(oldOutStockNum.subtract(oldOutNum));
            // 现有量 加上 old
            existStock.setStockNum(oldStockNum.add(oldOutNum));

            if(!erpStockService.updateById(existStock)){
                log.error("删除成品出库明细失败，调整库存数量失败,调用{}的{}方法出错", "ErpRepoReceiptDetailServiceImpl", "deleteRepoReceiptDetailProductOutById()");
                throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.ERP_REPOSITORY_RECEIPT_PRODUCT_OUT_DETAIL_DELETE_FAILED_UPDATE_STOCK_FAILED);

            }
        }else {

            log.info("删除成品出库明细时，调整库存量，没有找到相关记录");
        }

        if(!updateById(new ErpRepoReceiptDetail().setId(id).setDeleted(1))){
            log.error("删除成品出库明细失败，调用{}的{}方法出错", "ErpRepoReceiptDetailServiceImpl", "deleteRepoReceiptDetailProductOutById()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.ERP_REPOSITORY_RECEIPT_PRODUCT_OUT_DETAIL_DELETE_FAILED);
        }

        //更新订单状态
        updateSaleOrderStatus(re.getRelatedOrderId());

    }


    @Override
    public List<ErpRepoReceiptDetailProductOutDTO> getRepoReceiptDetailProductOutList(Integer receiptId) {


        return baseMapper.getRepoReceiptDetailProductOutList(receiptId);
    }


    /**
     * 根据id查询原料入库明细
     * @param id
     * @return
     */
    @Override
    public ErpRepoReceiptDetailRawInDTO getRepoReceiptDetailRawInById(Integer id) {
        ErpRepoReceiptDetailRawInDTO dto = new ErpRepoReceiptDetailRawInDTO();

        ErpRepoReceiptDetail d = selectOne(new EntityWrapper<ErpRepoReceiptDetail>().eq("id",id).eq("deleted",0));
        if(ObjectUtils.isEmpty(d)){
            log.info("根据id查看原料入库,没有找到相关数据，id为："+id);
            return dto;
        }
         dto = BeanUtils.copyProperties(d, ErpRepoReceiptDetailRawInDTO.class);

        Integer productId = dto.getProductId();
        ErpProduct p = productService.selectById(productId);
        if(!ObjectUtils.isEmpty(p)){
            dto.setProductName(p.getName()).setUnit(p.getUnit());
        }

        dto.setRepositoryName(commonMapper.getRepositoryNameById(d.getRepositoryId()));
        dto.setTestCode(commonMapper.getTestCodeById(d.getTestId()));
        dto.setResult(commonMapper.getTestResultById(d.getTestId()));

        return dto;
    }


    /**
     * 根据id查询成品出库明细
     * @param id
     * @return
     */
    @Override
    public ErpRepoReceiptDetailProductOutDTO getRepoReceiptDetailProductOutById(Integer id) {
        ErpRepoReceiptDetailProductOutDTO dto = new ErpRepoReceiptDetailProductOutDTO();

        ErpRepoReceiptDetail d = selectOne(new EntityWrapper<ErpRepoReceiptDetail>().eq("id",id).eq("deleted",0));
        if(ObjectUtils.isEmpty(d)){
            log.info("根据id查看成品出库,没有找到相关数据，id为："+id);
            return  dto;
        }
         dto = BeanUtils.copyProperties(d, ErpRepoReceiptDetailProductOutDTO.class);
        Integer productId = dto.getProductId();
        ErpProduct p = productService.selectById(productId);
        if(!ObjectUtils.isEmpty(p)){
            dto.setProductName(p.getName()).setUnit(p.getUnit());
        }

        dto.setRepositoryName(commonMapper.getRepositoryNameById(d.getRepositoryId()));
        dto.setTestCode(commonMapper.getTestCodeById(d.getTestId()));
        dto.setResult(commonMapper.getTestResultById(d.getTestId()));

        return dto;
    }

    /**
     * @Description 更新销售订单各操作状态
     * @Param orderId
     * @Date 2019/6/11 17:09
     * @Author xin.huang
     * @Return
     */
    private void updateSaleOrderStatus(Integer orderId) {
        //更新销售订单发货状态
        //合同数量
        List<ErpSaleOrderDetail> erpSaleOrderDetails = erpSaleOrderDetailMapper
                .selectList(new EntityWrapper<>(new ErpSaleOrderDetail()
                        .setOrderId(orderId).setDeleted(Status.FALSE.getKey())));
        if (CollectionUtils.isNotEmpty(erpSaleOrderDetails)) {
            BigDecimal totalNum = erpSaleOrderDetails.stream().map(ErpSaleOrderDetail::getNum)
                    .reduce(BigDecimal::add).orElse(BigDecimal.ZERO);
            //发货数量
            List<ErpRepoReceiptDetail> repoReceiptDetails = repoReceiptDetailMapper
                    .listByOrderId(EnumBusinessType.PRODUCT_DELIVERY.getCode(), orderId);
            if (CollectionUtils.isNotEmpty(repoReceiptDetails)) {
                BigDecimal deliverylNum = repoReceiptDetails.stream().map(ErpRepoReceiptDetail::getNum)
                        .reduce(BigDecimal::add).orElse(BigDecimal.ZERO);
                //若合同数量95%<发货数量<合同数量105%
                BigDecimal a = totalNum.multiply(new BigDecimal("0.95"));
                BigDecimal b = totalNum.multiply(new BigDecimal("1.05"));
                int deliveryState = EnumErpSaleOrderStatus.DeliveryType.N0T_DELIVERY.getKey();
                if (deliverylNum.compareTo(a) == 1 && deliverylNum.compareTo(b) != 1) {
                    deliveryState = EnumErpSaleOrderStatus.DeliveryType.ALL_DELIVERY.getKey();
                } else if (deliverylNum.compareTo(BigDecimal.ZERO) == 1){
                    deliveryState = EnumErpSaleOrderStatus.DeliveryType.PART_DELIVERY.getKey();
                }
                erpSaleOrderService.updateById(new ErpSaleOrder().setId(orderId).setDeliveryState(deliveryState));
            }
        }
    }
}
