package com.bee.platform.datadriver.service.impl;

import com.alibaba.fastjson.JSON;
import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.mapper.Wrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.constants.enums.EnumCommon;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.service.JedisService;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.DateUtils;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.datadriver.dao.mapper.ErpProductCategoryMapper;
import com.bee.platform.datadriver.dao.mapper.ErpProductMapper;
import com.bee.platform.datadriver.dto.*;
import com.bee.platform.datadriver.entity.*;
import com.bee.platform.datadriver.rq.ErpProductAddRQ;
import com.bee.platform.datadriver.rq.ErpProductBatchRQ;
import com.bee.platform.datadriver.rq.ErpProductListRQ;
import com.bee.platform.datadriver.rq.ErpProductUpdateRQ;
import com.bee.platform.datadriver.service.*;
import com.bee.platform.user.authority.dto.AuthEnterpriseFlatDTO;
import com.bee.platform.user.service.feign.AuthEnterpriseFeignClient;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;

import java.util.*;
import java.util.stream.Collectors;

/**
 * <p>
 * 产品档案 服务实现类
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
@Slf4j
@Service
public class ErpProductServiceImpl extends ServiceImpl<ErpProductMapper, ErpProduct> implements ErpProductService {

    @Autowired
    private ErpProductMapper productMapper;
    @Autowired
    private ErpProductCategoryMapper productCategoryMapper;
    @Autowired
    private JedisService jedisService;
    @Autowired
    private AuthEnterpriseFeignClient enterpriseFeignClient;
    @Autowired
    private CommonService commonService;
    @Autowired
    private ErpProductBatchService productBatchService;
    /**
     * ---判断是否使用产品的---
     */
    @Autowired
    private ErpTestReportService erpTestReportService;// 化验单
    @Autowired
    private ErpStockService erpStockService;// 库存表
    @Autowired
    private ErpMaterialBatchOrderService erpMaterialBatchOrderService;// 料批主表
    @Autowired
    private ErpMaterialBatchOrderDetailService erpMaterialBatchOrderDetailService;// 料批明细表
    @Autowired
    private ErpOpeningInventoryOrderDetailService erpOpeningInventoryOrderDetailService;// 期初库存明细表
    @Autowired
    private ErpOutOfStockOrderDetailService erpOutOfStockOrderDetailService;// 领料出库明细表
    @Autowired
    private ErpPurchaseInvoiceOrderDetailService erpPurchaseInvoiceOrderDetailService;// 采购发票明细
    @Autowired
    private ErpPurchaseOrderDetailService erpPurchaseOrderDetailService;// 采购单明细
    @Autowired
    private ErpPurchaseStmtDetailService erpPurchaseStmtDetailService;// 采购结算单结算详情
    @Autowired
    private ErpSaleInvoiceOrderDetailService erpSaleInvoiceOrderDetailService;// 销售发票明细
    @Autowired
    private ErpSaleOrderDetailService erpSaleOrderDetailService;// 销售单明细
    @Autowired
    private ErpSaleStmtDetailService erpSaleStmtDetailService;// 销售结算单结算详情
    @Autowired
    private ErpRepoReceiptDetailService erpRepoReceiptDetailService;// 原料入库，成品出库单明细
    @Autowired
    private ErpStockCheckDetailService erpStockCheckDetailService;// 库存盘点明细
    /**
     * 产品档案code前缀
     */
    private final String productCodePrefix = "PA";

    @Override
    @Transactional(rollbackFor = Exception.class)
    public boolean updateBatchById(List<ErpProduct> entityList) {
        return super.updateBatchById(entityList);
    }

    @Override
    public ResponseResult<String> generateProductCode(Integer orgId) {
        // 编码
        String redisKey = "siProduct" + orgId;
        int productInt = commonService.getRedisCode(redisKey);
        String sb = productCodePrefix
                + DateUtils.getCurrentDate()
                + commonService.getCode(productInt + "", 3);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, sb);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<Integer> addProduct(ErpProductAddRQ rq, AuthPlatformUserInfo userInfo) {
        Integer orgId = userInfo.getOrgId();
        // 判断产品名称是否重复
        if (checkNameRepeat(rq.getCategory(), userInfo.getOrgId(), rq.getName())) {
            return ResponseResult.buildResponseResult(ResCodeEnum.PRODUCT_NAME_EXIST);
        }
        // 复制属性
        ErpProduct product = BeanUtils.copyProperties(rq, ErpProduct.class);
        // 如果前端没传企业id 则默认使用当前登录人企业id
        if (ObjectUtils.isEmpty(rq.getEnterpriseId()) || rq.getEnterpriseId().equals(0)) {
            product.setEnterpriseId(orgId);
        }
        product.setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())
                .setCheckItems(JSON.toJSONString(rq.getCheckItems()))
                .setLogo(JSON.toJSONString(rq.getLogo()))
                .setCreateTime(new Date())
                .setOperateId(userInfo.getId());
        if (productMapper.insert(product) != 1) {
            log.error("新增产品失败，类：{} 方法：{}", "ErpProductServiceImpl", "addProduct");
            return ResponseResult.buildResponseResult(ResCodeEnum.ADD_FAILED);
        }
        /**------插入批次信息-----*/
        // 如果启用了批次
        if (Status.TRUE.getKey().equals(rq.getEnableBatch())) {
            List<ErpProductBatchRQ> productBatchRQList = rq.getProductBatchList();
            if (!CollectionUtils.isEmpty(productBatchRQList)) {
                List<ErpProductBatch> productBatchList = BeanUtils.assemble(ErpProductBatch.class, productBatchRQList);
                productBatchList.forEach(
                        a -> a.setCreatorId(userInfo.getId())
                                .setCreateTime(new Date())
                                .setDeleted(Status.FALSE.getKey())
                                .setStatus(Status.TRUE.getKey())
                                .setProductId(product.getId()));
                if (!productBatchService.insertBatch(productBatchList)) {
                    log.error("新增产品批次失败，类：{} 方法：{}", "ErpProductServiceImpl", "addProduct");
                    return ResponseResult.buildResponseResult(ResCodeEnum.ADD_FAILED);
                }
            }
        }
        // 把增加后的自增序号写回redis
        String redisKey = "siProduct" + orgId;
        String code = rq.getCode();
        if (!org.apache.commons.lang3.StringUtils.isBlank(code)) {
            String intString = code.substring(10);
            jedisService.set(redisKey, (Integer.valueOf(intString) + 1) + "", 0);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, product.getId());
    }

    /**
     * 检测产品名称是否重复
     *
     * @param category 产品分类
     * @param orgId    企业id
     * @param name     产品名称
     * @return
     */
    private boolean checkNameRepeat(int category, int orgId, String name) {
        int productNameCheckCount = this.selectCount(new EntityWrapper<>(new ErpProduct()
                .setEnterpriseId(orgId)
                .setCategory(category)
                .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())
                .setName(name)));
        return productNameCheckCount > 0;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<Integer> deleteProduct(Integer id, AuthPlatformUserInfo userInfo) {
        Integer orgId = userInfo.getOrgId();
        ErpProduct product = productMapper.selectOne(new ErpProduct().setId(id).setEnterpriseId(orgId));
        if (ObjectUtils.isEmpty(product)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.PRODUCT_NOT_EXIST);
        }
        // 判断产品是否使用
        if (productUserd(id)) {
            log.info("产品已使用不能删除，类：{} 方法：{}", "ErpProductServiceImpl", "deleteProduct");
            return ResponseResult.buildResponseResult(ResCodeEnum.PRODUCT_IS_USED);
        }
        product.setOperateId(userInfo.getId())
                .setDeletedTime(new Date())
                .setDeleted(EnumCommon.IsDeleted.is_Delete.getKey());
        if (productMapper.updateById(product) != 1) {
            log.error("删除产品失败，类：{} 方法：{}", "ErpProductServiceImpl", "deleteProduct");
            return ResponseResult.buildResponseResult(ResCodeEnum.DELETE_FAIL);
        }
        // 删除产品批次信息
        productBatchService.update(new ErpProductBatch().setDeleted(Status.TRUE.getKey()),
                new EntityWrapper<>(new ErpProductBatch().setProductId(id)));

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, product.getId());
    }

    /**
     * 判断产品是否被使用过
     *
     * @param id
     * @return
     */
    private boolean productUserd(Integer id) {
        // 化验单
        int count1 = erpTestReportService.selectCount(new EntityWrapper<>(new ErpTestReport().setDeleted(Status.FALSE.getKey()).setProduct(id)));
        if (count1 > 0) {
            return true;
        }
        // 库存表
        int count2 = erpStockService.selectCount(new EntityWrapper<>(new ErpStock().setDeleted(Status.FALSE.getKey()).setProductId(id)));
        if (count2 > 0) {
            return true;
        }
        //库存盘点明细
        int count14 = erpStockCheckDetailService.selectCount(new EntityWrapper<>(new ErpStockCheckDetail().setDeleted(Status.FALSE.getKey()).setProductId(id)));
        if (count14 > 0) {
            return true;
        }
        // 料批主表
        int count3 = erpMaterialBatchOrderService.selectCount(new EntityWrapper<>(new ErpMaterialBatchOrder().setDeleted(Status.FALSE.getKey()).setProductId(id)));
        if (count3 > 0) {
            return true;
        }
        // 料批明细表
        int count4 = erpMaterialBatchOrderDetailService.selectCount(new EntityWrapper<>(new ErpMaterialBatchOrderDetail().setDeleted(Status.FALSE.getKey()).setProductId(id)));
        if (count4 > 0) {
            return true;
        }
        // 期初库存明细表
        int count5 = erpOpeningInventoryOrderDetailService.selectCount(new EntityWrapper<>(new ErpOpeningInventoryOrderDetail().setDeleted(Status.FALSE.getKey()).setProductId(id)));
        if (count5 > 0) {
            return true;
        }
        // 领料出库明细表
        int count6 = erpOutOfStockOrderDetailService.selectCount(new EntityWrapper<>(new ErpOutOfStockOrderDetail().setDeleted(Status.FALSE.getKey()).setProductId(id)));
        if (count6 > 0) {
            return true;
        }
        // 采购发票明细
        int count7 = erpPurchaseInvoiceOrderDetailService.selectCount(new EntityWrapper<>(new ErpPurchaseInvoiceOrderDetail().setDeleted(Status.FALSE.getKey()).setProductId(id)));
        if (count7 > 0) {
            return true;
        }
        //  采购单明细
        int count8 = erpPurchaseOrderDetailService.selectCount(new EntityWrapper<>(new ErpPurchaseOrderDetail().setDeleted(Status.FALSE.getKey()).setProductId(id)));
        if (count8 > 0) {
            return true;
        }
        //  采购结算单结算详情
        int count9 = erpPurchaseStmtDetailService.selectCount(new EntityWrapper<>(new ErpPurchaseStmtDetail().setDeleted(Status.FALSE.getKey()).setProductId(id)));
        if (count9 > 0) {
            return true;
        }
        // 销售发票明细
        int count10 = erpSaleInvoiceOrderDetailService.selectCount(new EntityWrapper<>(new ErpSaleInvoiceOrderDetail().setDeleted(Status.FALSE.getKey()).setProductId(id)));
        if (count10 > 0) {
            return true;
        }
        // 销售单明细
        int count11 = erpSaleOrderDetailService.selectCount(new EntityWrapper<>(new ErpSaleOrderDetail().setDeleted(Status.FALSE.getKey()).setProductId(id)));
        if (count11 > 0) {
            return true;
        }
        // 销售结算单结算详情
        int count12 = erpSaleStmtDetailService.selectCount(new EntityWrapper<>(new ErpSaleStmtDetail().setDeleted(Status.FALSE.getKey()).setProduct(id)));
        if (count12 > 0) {
            return true;
        }
        // 原料入库，成品出库单明细
        int count13 = erpRepoReceiptDetailService.selectCount(new EntityWrapper<>(new ErpRepoReceiptDetail().setDeleted(Status.FALSE.getKey()).setProductId(id)));
        return count13 > 0;
    }


    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<Integer> updateProduct(ErpProductUpdateRQ rq, AuthPlatformUserInfo userInfo) {
        Integer orgId = userInfo.getOrgId();
        Integer productId = rq.getId();
        ErpProduct product = this.selectOne(new EntityWrapper<>(new ErpProduct().setId(productId).setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())));
        if (ObjectUtils.isEmpty(product)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.PRODUCT_NOT_EXIST);
        }
        // 如果修改了名称 则判断名称是否重复
        if (!rq.getName().equals(product.getName())) {
            if (checkNameRepeat(rq.getCategory(), userInfo.getOrgId(), rq.getName())) {
                return ResponseResult.buildResponseResult(ResCodeEnum.PRODUCT_NAME_EXIST);
            }
        }
        org.springframework.beans.BeanUtils.copyProperties(rq, product);
        product.setOperateId(orgId)
                .setUpdateTime(new Date())
                .setLogo(JSON.toJSONString(rq.getLogo()))
                .setCheckItems(JSON.toJSONString(rq.getCheckItems()));
        if (productMapper.updateById(product) != 1) {
            return ResponseResult.buildResponseResult(ResCodeEnum.UPDATE_FAIL);
        }
        // 判断是否启用批次
        if (Status.TRUE.getKey().equals(rq.getEnableBatch())) {
            List<ErpProductBatchRQ> batchRqList = rq.getProductBatchList();
            // 判断批次信息是否修改
            if (checkBatchChange(productId, batchRqList)) {
                // 判断产品是否使用,已使用不允许修改批次
                if (productUserd(productId)) {
                    log.info("产品已使用不能修改批次，类：{} 方法：{}", "ErpProductServiceImpl", "updateProduct");
                    return ResponseResult.buildResponseResult(ResCodeEnum.PRODUCT_IS_USED_BATCH_NOT_UPDATE);
                }
                // 修改批次信息---先逻辑删除，在插入
                if (!updateBatch(productId, batchRqList, userInfo.getId())) {
                    log.error("更新产品--更新批次失败，类：{} 方法：{}", "ErpProductServiceImpl", "updateProduct");
                    return ResponseResult.buildResponseResult(ResCodeEnum.UPDATE_FAIL);
                }
            }
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, product.getId());
    }

    /**
     * 更新产品---更新批次
     *
     * @param productId   产品id
     * @param batchRqList 前端传入批次rq
     * @param userId      用户id
     * @return
     */
    private boolean updateBatch(int productId, List<ErpProductBatchRQ> batchRqList, int userId) {
        // 删除以前批次信息
        productBatchService.update(new ErpProductBatch().setDeleted(Status.TRUE.getKey()),
                new EntityWrapper<>(new ErpProductBatch().setProductId(productId)));
        // 插入新的批次信息
        List<ErpProductBatch> batchList = Lists.newArrayList();
        for (ErpProductBatchRQ batchRQ : batchRqList) {
            batchList.add(new ErpProductBatch().setBatchName(batchRQ.getBatchName())
                    .setDeleted(Status.FALSE.getKey())
                    .setStatus(Status.TRUE.getKey())
                    .setProductId(productId)
                    .setCreatorId(userId)
                    .setCreateTime(new Date())
            );
        }
        return productBatchService.insertBatch(batchList);
    }

    /**
     * 更新产品时，判断批次是否修改
     *
     * @param productId   产品id
     * @param batchRqList 前端传入批次信息
     * @return 修改返回true
     */
    private boolean checkBatchChange(int productId, List<ErpProductBatchRQ> batchRqList) {
        List<ErpProductBatch> batchList = productBatchService.selectList(new EntityWrapper<>(new ErpProductBatch()
                .setDeleted(Status.FALSE.getKey())
                .setStatus(Status.TRUE.getKey())
                .setProductId(productId)
        ));
        List<String> batchRqNameList = Lists.newArrayList();
        List<String> batchNameList = Lists.newArrayList();
        if (!CollectionUtils.isEmpty(batchRqList)) {
            batchRqNameList = batchRqList.stream().map(a -> a.getBatchName()).collect(Collectors.toList());
        }
        if (!CollectionUtils.isEmpty(batchList)) {
            batchNameList = batchList.stream().map(a -> a.getBatchName()).collect(Collectors.toList());
        }
        return !batchRqNameList.equals(batchNameList);
    }

    /**
     * 禁用/启用产品
     */
    @Override
    public ResponseResult<Integer> updateStatus(Integer id, Integer status, AuthPlatformUserInfo userInfo) {
        ErpProduct product = this.selectOne(new EntityWrapper<>(new ErpProduct().setId(id)));
        if (ObjectUtils.isEmpty(product)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.PRODUCT_NOT_EXIST);
        }
        product.setStatus(status).setUpdateTime(new Date()).setOperateId(userInfo.getId());
        if (!this.updateById(product)) {
            log.info("更新产品启用状态失败！类：{} 方法：{}", "ErpProductServiceImpl", "updateStatus");
            return ResponseResult.buildResponseResult(ResCodeEnum.UPDATE_FAIL);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, product.getId());
    }

    @Override
    public ResponseResult<List<ErpProductListDTO>> getProductConditional(ErpProductListRQ rq, Page page, Integer companyId) {
        Pagination pagination = PageUtils.transFromPage(page);
        // 构建条件
        EntityWrapper<ErpProduct> wrapper = new EntityWrapper<>();
        // 查询当前公司及子公司
        List<AuthEnterpriseFlatDTO> enterpriseFlatDTOS = enterpriseFeignClient.getEnterpriseFlatByCompanyId(companyId).getObject();
        Map<Integer, String> enterpriseMap = Maps.newHashMap();
        if (!CollectionUtils.isEmpty(enterpriseFlatDTOS)) {
//            List<Integer> ids = enterpriseFlatDTOS.stream().map(a -> a.getValue()).collect(Collectors.toList());
            enterpriseMap = enterpriseFlatDTOS.stream().collect(Collectors.toMap(a -> a.getValue(), b -> b.getLabel()));
//            wrapper.in("enterprise_id", ids);
        }
//       else {
//            wrapper.eq("enterprise_id", companyId);
//        }
        wrapper.eq("enterprise_id", companyId);
        if (!StringUtils.isEmpty(rq.getName())) {
            wrapper.like("name", rq.getName());
        }
        if (!ObjectUtils.isEmpty(rq.getCategory())) {
            wrapper.eq("category", rq.getCategory());
        }
        if (!ObjectUtils.isEmpty(rq.getStatus())) {
            wrapper.eq("status", rq.getStatus());
        }
        wrapper.setEntity(new ErpProduct().setDeleted(EnumCommon.IsDeleted.not_Delete.getKey()));
        wrapper.orderBy("create_time", false);
        List<ErpProduct> products = productMapper.selectPage(pagination, wrapper);
        List<ErpProductListDTO> productDTOS = BeanUtils.assemble(ErpProductListDTO.class, products);
        // 查询产品类别名称
        List<ErpProductCategory> productCategoryList = productCategoryMapper.selectList(new EntityWrapper<>(new ErpProductCategory()
                .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())
                .setStatus(EnumCommon.IsActive.is_active.getKey())));
        Map<Integer, String> categoryMap = productCategoryList.stream().collect(Collectors.toMap(a -> a.getId(), b -> b.getName()));
        for (ErpProductListDTO dto : productDTOS) {
            dto.setCategoryName(categoryMap.get(dto.getCategory()))
                    .setEnterprise(enterpriseMap.get(dto.getEnterpriseId()));
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, productDTOS, PageUtils.transToPage(pagination));
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ErpProductDetailDTO> getById(Integer id) {
        ErpProduct product = this.selectOne(new EntityWrapper<>(new ErpProduct()
                .setId(id)
                .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())));
        if (ObjectUtils.isEmpty(product)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.PRODUCT_NOT_EXIST);
        }
        ErpProductDetailDTO productDetailDTO = BeanUtils.copyProperties(product, ErpProductDetailDTO.class);
        // logo
        if (product.getLogo() != null) {
            productDetailDTO.setLogo(JSON.parseArray(product.getLogo(), ErpLogoDTO.class));
        }
        // 检测属性
        if (product.getCheckItems() != null) {
            productDetailDTO.setCheckItems(JSON.parseArray(product.getCheckItems(), ErpProductCheckItemsDTO.class));
        }
        // 批次启用---查询产品批次
        if (Status.TRUE.getKey().equals(product.getEnableBatch())) {
            List<ErpProductBatch> productBatchList = productBatchService.selectList(new EntityWrapper<>(new ErpProductBatch()
                    .setProductId(id)
                    .setStatus(Status.TRUE.getKey())
                    .setDeleted(Status.FALSE.getKey())));
            if (!CollectionUtils.isEmpty(productBatchList)) {
                List<ErpProductBatchDTO> productBatchDTOList = BeanUtils.assemble(ErpProductBatchDTO.class, productBatchList);
                productDetailDTO.setProductBatchList(productBatchDTOList);
            }
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, productDetailDTO);
    }

    /**
     * 当前用户所在企业的产品
     */
    @Override
    public ResponseResult<List<ErpProductBoxTreeDTO>> getEnterpriseProduct(Integer orgId) {
        List<Integer> ids = Lists.newArrayList();
        ids.add(orgId);
        return getProduct(ids);
    }

    private ResponseResult<List<ErpProductBoxTreeDTO>> getProduct(List<Integer> orgIds) {
        if (CollectionUtils.isEmpty(orgIds)) {
            log.info("企业id集合为空，类：{} 方法：{}", "ErpProductServiceImpl", "getProduct");
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, new ArrayList<>(0));
        }
        List<ErpProduct> productList = selectList(new EntityWrapper<>(new ErpProduct()
                .setStatus(EnumCommon.IsActive.is_active.getKey())
                .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey()))
                .in("enterprise_id", orgIds));
        if (CollectionUtils.isEmpty(productList)) {
            log.info("公司产品为空，类：{} 方法：{}", "ErpProductServiceImpl", "getProduct");
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, new ArrayList<>(0));
        }
        List<ErpProductBoxTreeDTO> boxDTOS = BeanUtils.assemble(ErpProductBoxTreeDTO.class, productList);
        Set<Integer> productIds = productList.stream().map(a -> a.getId()).collect(Collectors.toSet());
        // 获取产品的批次信息
        getProductBatch(productIds, boxDTOS);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, boxDTOS);
    }

    private void getProductBatch(Set<Integer> productIds, List<ErpProductBoxTreeDTO> boxDTOS) {
        List<ErpProductBatch> batchList = productBatchService.selectList(new EntityWrapper<>(new ErpProductBatch()
                .setStatus(EnumCommon.IsActive.is_active.getKey())
                .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey()))
                .in("product_id", productIds));
        Iterator<ErpProductBatch> iterator = batchList.iterator();
        for (ErpProductBoxTreeDTO dto : boxDTOS) {
            for (ErpProductBatch batch : batchList) {
                if (dto.getId().equals(batch.getProductId())) {
                    if (CollectionUtils.isEmpty(dto.getBatchList())) {
                        dto.setBatchList(Lists.newArrayList());
                    }
                    dto.getBatchList().add(new ErpProductBatchDTO()
                            .setId(batch.getId())
                            .setBatchName(batch.getBatchName()));
                }
            }
        }
    }

    /**
     * 根据分类查询当前用户所在企业的产品
     */
    @Override
    public ResponseResult<List<ErpProductBoxTreeDTO>> getEnterpriseProductByCategory(Integer orgId, List<Integer> categoryList) {
        List<Integer> ids = Lists.newArrayList();
        ids.add(orgId);
        return getProductByCategory(ids, categoryList);
    }

    private ResponseResult<List<ErpProductBoxTreeDTO>> getProductByCategory(List<Integer> orgIds, List<Integer> categoryList) {
        Wrapper<ErpProduct> wrapper = new EntityWrapper<>();
        wrapper.in("enterprise_id", orgIds)
                .eq("status", 1)
                .eq("deleted", 0);
        if (!CollectionUtils.isEmpty(categoryList) && categoryList.size() > 0) {
            wrapper.in("category", categoryList);
        }
        List<ErpProduct> productList = this.selectList(wrapper);
        if (CollectionUtils.isEmpty(productList)) {
            log.info("公司产品为空，类：{} 方法：{}", "ErpProductServiceImpl", "getProduct");
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, new ArrayList<>(0));
        }
        List<ErpProductBoxTreeDTO> boxDTOS = BeanUtils.assemble(ErpProductBoxTreeDTO.class, productList);
        Set<Integer> productIds = productList.stream().map(a -> a.getId()).collect(Collectors.toSet());
        // 获取产品的批次信息
        getProductBatch(productIds, boxDTOS);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, boxDTOS);
    }

    //    /**
//     * 当前用户所在企业及-子企业的产品
//     */
//    @Override
//    public ResponseResult<List<ErpProductBoxDTO>> getEnterpriseSubProduct(AuthPlatformUserInfo userInfo) {
//        List<AuthEnterpriseFlatDTO> enterpriseFlatDTOS = enterpriseFeignClient.getEnterpriseFlatByCompanyId(userInfo.getOrgId()).getObject();
//        List<Integer> ids = null;
//        if (CollectionUtils.isEmpty(enterpriseFlatDTOS)) {
//            ids = Lists.newArrayList();
//            ids.add(userInfo.getOrgId());
//        } else {
//            ids = enterpriseFlatDTOS.stream().map(a -> a.getValue()).collect(Collectors.toList());
//        }
//        return getProduct(ids, userInfo);
//    }
//
//    /**
//     * 当前用户所在企业及-集团下所有公司的产品
//     */
//    @Override
//    public ResponseResult<List<ErpProductBoxDTO>> getEnterpriseGroupProduct(AuthPlatformUserInfo userInfo) {
//        List<AuthEnterpriseFlatDTO> enterpriseFlatDTOS = enterpriseFeignClient.getParentSubEnterpriseFlat(userInfo.getSysToken()).getObject();
//        List<Integer> ids = null;
//        if (CollectionUtils.isEmpty(enterpriseFlatDTOS)) {
//            ids = Lists.newArrayList();
//            ids.add(userInfo.getOrgId());
//        } else {
//            ids = enterpriseFlatDTOS.stream().map(a -> a.getValue()).collect(Collectors.toList());
//        }
//        return getProduct(ids, userInfo);
//    }

    /**
     * 通过产品id获取产品名称和产品分类名称,产品分类id
     */
    @Override
    public ErpProductCategoryNameDTO getProductCategoryName(Integer productId) {
        return baseMapper.getProductCategory(productId);
    }

    /**
     * 根据产品id获取 该产品的检测属性
     */
    @Override
    public ResponseResult<List<ErpProductCheckItemsDTO>> getCheckItems(Integer id) {
        ErpProduct product = this.selectOne(new EntityWrapper<>(new ErpProduct().setId(id)));
        if (ObjectUtils.isEmpty(product)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.PRODUCT_NOT_EXIST);
        }
        List<ErpProductCheckItemsDTO> checkItems = JSON.parseArray(product.getCheckItems(), ErpProductCheckItemsDTO.class);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, checkItems);
    }

    /**
     * 根据产品分类查询产品列表
     *
     * @param orgId    企业id
     * @param category 分类id
     * @return 产品列表
     * @Author ck
     */
    @Override
    public List<ErpProductListByCategoryDTO> getProductListByCategory(Integer orgId, Integer category) {
        return baseMapper.getProductListByCategory(orgId, category);
    }

}
