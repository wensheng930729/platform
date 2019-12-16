package com.bee.platform.datadriver.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.common.utils.Validator;
import com.bee.platform.datadriver.dao.mapper.*;
import com.bee.platform.datadriver.dto.*;
import com.bee.platform.datadriver.entity.*;
import com.bee.platform.datadriver.enums.EnumBusinessType;
import com.bee.platform.datadriver.enums.EnumErpSaleOrderStatus;
import com.bee.platform.datadriver.enums.EnumPurchaseGoodsOrderStatus;
import com.bee.platform.datadriver.rq.*;
import com.bee.platform.datadriver.service.*;
import com.bee.platform.datadriver.utils.GenerateIdUtils;
import com.bee.platform.user.authority.dto.AuthEnterpriseFlatDTO;
import com.bee.platform.user.service.feign.AuthEnterpriseFeignClient;
import com.google.common.collect.Lists;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * <p>
 *  服务实现类
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
@Slf4j
@Service
public class ErpRepositoryReceiptServiceImpl extends ServiceImpl<ErpRepositoryReceiptMapper, ErpRepositoryReceipt> implements ErpRepositoryReceiptService {

    @Autowired
    private GenerateIdUtils generateIdUtils;

    @Autowired
    private ErpRepositoryReceiptMapper repositoryReceiptMapper;
    @Autowired
    private ErpRepoReceiptDetailService repoReceiptDetailService;
    @Autowired
    private ErpRepoReceiptDetailMapper repoReceiptDetailMapper;
    @Autowired
    private ErpPurchaseOrderDetailMapper purchaseOrderDetailMapper;
    @Autowired
    private ErpPurchaseOrderMapper purchaseOrderMapper;

    @Autowired
    private ErpPurchaseOrderService erpPurchaseOrderService;

    @Autowired
    private ErpSaleOrderService erpSaleOrderService;

    @Autowired
    private ErpProductMapper productMapper;
    @Autowired
    private AuthEnterpriseFeignClient authEnterpriseFeignClient;

    @Autowired
    private CommonMapper commonMapper;

    @Autowired
    private ErpProductService productService;


    /**
     * @Description 新增采购收货单
     * @Param repositoryReceiptRQ
     * @Date 2019/5/28 17:17
     * @Author xin.huang
     * @Return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<Integer> addPurchaseGoodsOrder(AuthPlatformUserInfo simpleUserInfo, RepositoryReceiptRQ repositoryReceiptRQ) {
        if (!Validator.validateCode(repositoryReceiptRQ.getCode())){
            return ResponseResult.buildResponseResult(ResCodeEnum.PURCHASE_GOODS_ORDER_CODE_ERROR);
        }
        ErpRepositoryReceipt repositoryReceipt = selectOne(new EntityWrapper<>(new ErpRepositoryReceipt()
                .setCode(repositoryReceiptRQ.getCode())
                .setDeleted(Status.FALSE.getKey())
                .setOrgId(repositoryReceiptRQ.getCompanyId())));
        if (Objects.nonNull(repositoryReceipt)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.PURCHASE_GOODS_ORDER_CODE_EXIST);
        }
        ErpRepositoryReceipt erpRepositoryReceipt = BeanUtils.copyProperties(repositoryReceiptRQ, ErpRepositoryReceipt.class);
        erpRepositoryReceipt.setCreateUser(simpleUserInfo.getId())
                .setEnterpriseId(simpleUserInfo.getOrgId())
                .setState(EnumPurchaseGoodsOrderStatus.SAVED.getKey())
                .setType(EnumBusinessType.MATERIAL_STOCK.getCode())
                .setOrgId(repositoryReceiptRQ.getCompanyId())
                .setCreateTime(new Date())
                .setUpdateTime(new Date());
        repositoryReceiptMapper.insert(erpRepositoryReceipt);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, erpRepositoryReceipt.getId());
    }

    /**
     * @Description 更新采购收货单
     * @Param id
     * @Param repositoryReceiptRQ
     * @Date 2019/5/29 11:39
     * @Author xin.huang
     * @Return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<Integer> updatePurchaseGoodsOrder(AuthPlatformUserInfo simpleUserInfo, RepositoryReceiptRQ repositoryReceiptRQ) {
        ErpRepositoryReceipt erpRepositoryReceipt = repositoryReceiptMapper
                .selectOne(new ErpRepositoryReceipt().setId(repositoryReceiptRQ.getId()).setDeleted(Status.FALSE.getKey()));
        if (Objects.isNull(erpRepositoryReceipt)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.PURCHASE_GOODS_ORDER_NOT_EXIST);
        }
        ErpRepositoryReceipt repositoryReceipt = BeanUtils.copyProperties(repositoryReceiptRQ, ErpRepositoryReceipt.class);
        repositoryReceiptMapper.updateById(repositoryReceipt);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, repositoryReceiptRQ.getId());
    }

    /**
     * @Description 批量删除采购收货单
     * @Param ids
     * @Date 2019/5/29 15:43
     * @Author xin.huang
     * @Return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> batchDeletePurchaseGoodsOrder(String ids) {
        String[] idArray = ids.split(",");
        if (idArray.length <= 0) {
            return ResponseResult.buildResponseResult(ResCodeEnum.PURCHASE_GOODS_ORDER_ID_EMPTY);
        }
        List<ErpRepositoryReceipt> repositoryReceipts = new ArrayList<ErpRepositoryReceipt>();
        ErpRepositoryReceipt repositoryReceipt;
        Integer receiptId;
        for (String id : idArray) {
            receiptId = Integer.valueOf(id);
            repositoryReceipt = new ErpRepositoryReceipt();
            repositoryReceipt.setId(receiptId);
            repositoryReceipt.setDeleted(Status.TRUE.getKey());
            repositoryReceipts.add(repositoryReceipt);
        }
        //删除采购收货单
        this.updateBatchById(repositoryReceipts);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * @Description 查看采购收货单
     * @Param id
     * @Date 2019/5/29 16:35
     * @Author xin.huang
     * @Return
     */
    @Override
    public ErpPurchaseGoodsOrderDTO findPurchaseGoodsOrder(Integer id) {
        ErpRepositoryReceipt repositoryReceipt = repositoryReceiptMapper
                .selectOne(new ErpRepositoryReceipt().setId(id).setDeleted(Status.FALSE.getKey()));
        if (Objects.isNull(repositoryReceipt)) {
            throw new BusinessException(ResCodeEnum.ERROR_SYSTEM, ExceptionMessageEnum.PURCHASEGOODSORDER_NOT_EXIST);
        }
        return BeanUtils.copyProperties(repositoryReceipt, ErpPurchaseGoodsOrderDTO.class);
    }

    /**
     * @Description 条件查询采购收货单列表
     * @Param purchaseGoodsOrderSelectRQ
     * @Param pagination
     * @Date 2019/5/30 14:45
     * @Author xin.huang
     * @Return
     */
    @Override
    public List<ErpPurchaseGoodsDetailDTO> findPurchaseGoodsOrderList(AuthPlatformUserInfo simpleUserInfo,
                                                                      ErpPurchaseGoodsOrderSelectRQ purchaseGoodsOrderSelectRQ,
                                                                      Pagination pagination) {
        if (Objects.isNull(purchaseGoodsOrderSelectRQ.getCompanyId())) {
            List<AuthEnterpriseFlatDTO> enterprises = authEnterpriseFeignClient
                    .getEnterpriseFlatByUser(simpleUserInfo.getSysToken()).getObject();
            if (!CollectionUtils.isEmpty(enterprises)) {
                List<Integer> enterpriseIds = new ArrayList<Integer>();
                enterprises.forEach(enterprise -> {
                    enterpriseIds.add(enterprise.getValue());
                });
                purchaseGoodsOrderSelectRQ.setEnterpriseIdList(enterpriseIds);
            }
        }
        return repositoryReceiptMapper.findPurchaseGoodsOrderList(purchaseGoodsOrderSelectRQ, pagination);
    }

    /**
     * @Description 更新采购收货单状态
     * @Param id
     * @Param state
     * @Date 2019/5/30 15:23
     * @Author xin.huang
     * @Return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<Integer> updateState(AuthPlatformUserInfo simpleUserInfo, Integer id, Integer state) {
        ErpRepositoryReceipt repositoryReceipt = repositoryReceiptMapper.selectOne(new ErpRepositoryReceipt()
                .setId(id).setDeleted(Status.FALSE.getKey()));
        if (Objects.isNull(repositoryReceipt)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.PURCHASE_GOODS_ORDER_NOT_EXIST);
        }
        repositoryReceiptMapper.update(new ErpRepositoryReceipt().setState(state)
                        .setUpdateUser(simpleUserInfo.getId()),
                new EntityWrapper<ErpRepositoryReceipt>().eq("id", id));
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);
    }

    /**
     * @Description 新增销售发货单
     * @Param repositoryReceiptRQ
     * @Date 2019/5/31 10:01
     * @Author xin.huang
     * @Return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<Integer> addStatementDeliveryOrder(AuthPlatformUserInfo simpleUserInfo, RepositoryReceiptRQ repositoryReceiptRQ) {
        if (!Validator.validateCode(repositoryReceiptRQ.getCode())){
            return ResponseResult.buildResponseResult(ResCodeEnum.STATEMENT_DELIVERY_CODE_ERROR);
        }
        ErpRepositoryReceipt repositoryReceipt = this.selectOne(new EntityWrapper<>(new ErpRepositoryReceipt()
                .setCode(repositoryReceiptRQ.getCode())
                .setDeleted(Status.FALSE.getKey())
                .setOrgId(repositoryReceiptRQ.getCompanyId())
                .setCreateTime(new Date())
                .setUpdateTime(new Date())));
        if (Objects.nonNull(repositoryReceipt)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.STATEMENT_DELIVERY_CODE_EXIST);
        }
        ErpRepositoryReceipt erpRepositoryReceipt = BeanUtils.copyProperties(repositoryReceiptRQ, ErpRepositoryReceipt.class);
        erpRepositoryReceipt.setState(EnumPurchaseGoodsOrderStatus.SAVED.getKey())
                .setCreateUser(simpleUserInfo.getId())
                .setOrgId(repositoryReceiptRQ.getCompanyId())
                .setType(EnumBusinessType.PRODUCT_DELIVERY.getCode())
                .setEnterpriseId(simpleUserInfo.getOrgId());
        repositoryReceiptMapper.insert(erpRepositoryReceipt);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, erpRepositoryReceipt.getId());
    }

    /**
     * @Description 更新销售发货单
     * @Param id
     * @Param repositoryReceiptRQ
     * @Date 2019/5/31 10:18
     * @Author xin.huang
     * @Return
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public ResponseResult<Integer> updateStatementDeliveryOrder(AuthPlatformUserInfo simpleUserInfo, RepositoryReceiptRQ repositoryReceiptRQ) {
        ErpRepositoryReceipt erpRepositoryReceipt = repositoryReceiptMapper
                .selectOne(new ErpRepositoryReceipt().setId(repositoryReceiptRQ.getId()).setDeleted(Status.FALSE.getKey()));
        if (Objects.isNull(erpRepositoryReceipt)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.STATEMENT_DELIVERY_ORDER_NOT_EXIST);
        }
        repositoryReceiptMapper.updateById(new ErpRepositoryReceipt().setId(repositoryReceiptRQ.getId())
                .setUrl(repositoryReceiptRQ.getUrl())
                .setRemark(repositoryReceiptRQ.getRemark())
                .setUpdateUser(simpleUserInfo.getId()));
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, repositoryReceiptRQ.getId());
    }

    /**
     * @Description 批量删除销售发货单
     * @Param ids
     * @Date 2019/5/31 10:23
     * @Author xin.huang
     * @Return
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public ResponseResult<ResCodeEnum> batchStatementDeliveryOrder(String ids) {
        String[] idArray = ids.split(",");
        if (idArray.length <= 0) {
            return ResponseResult.buildResponseResult(ResCodeEnum.STATEMENT_DELIVERY_ORDER_ID_EMPTY);
        }
        List<ErpRepositoryReceipt> repositoryReceipts = new ArrayList<ErpRepositoryReceipt>();
        ErpRepositoryReceipt repositoryReceipt;
        List<Integer> receiptIds = new ArrayList<Integer>();
        Integer receiptId;
        for (String id : idArray) {
            receiptId = Integer.valueOf(id);
            repositoryReceipt = new ErpRepositoryReceipt();
            repositoryReceipt.setId(receiptId);
            repositoryReceipt.setDeleted(Status.TRUE.getKey());
            repositoryReceipts.add(repositoryReceipt);
            receiptIds.add(receiptId);
        }
        //删除采购收货单
        this.updateBatchById(repositoryReceipts);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * @Description 查看销售发货单
     * @Param id
     * @Date 2019/5/29 16:35
     * @Author xin.huang
     * @Return
     */
    @Override
    public ResponseResult<ErpPurchaseGoodsOrderDTO> findStatementDeliveryOrder(Integer id) {
        ErpRepositoryReceipt repositoryReceipt = repositoryReceiptMapper
                .selectOne(new ErpRepositoryReceipt().setId(id).setDeleted(Status.FALSE.getKey()));
        if (Objects.isNull(repositoryReceipt)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.STATEMENT_DELIVERY_ORDER_NOT_EXIST);
        }
        ErpPurchaseGoodsOrderDTO purchaseGoodsOrderDto = BeanUtils.copyProperties(repositoryReceipt, ErpPurchaseGoodsOrderDTO.class);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, purchaseGoodsOrderDto);
    }

    /**
     * @Description 条件查询销售发货单列表
     * @Param purchaseGoodsOrderSelectRQ
     * @Param pagination
     * @Date 2019/5/30 14:45
     * @Author xin.huang
     * @Return
     */
    @Override
    public List<ErpPurchaseGoodsDetailDTO> findStatementDeliveryOrderList(ErpStatementDeliveryOrderSelectRQ statementDeliveryOrderSelectRQ, Pagination pagination,String sysToken) {
        if (Objects.isNull(statementDeliveryOrderSelectRQ.getCompanyId())) {
            List<AuthEnterpriseFlatDTO> enterprises = authEnterpriseFeignClient.getEnterpriseFlatByUser(sysToken).getObject();
            if (!CollectionUtils.isEmpty(enterprises)) {
                List<Integer> enterpriseIds = new ArrayList<Integer>();
                enterprises.forEach(enterprise -> {
                    enterpriseIds.add(enterprise.getValue());
                });
                statementDeliveryOrderSelectRQ.setEnterpriseIdList(enterpriseIds);
            }
        }
        return repositoryReceiptMapper.findStatementDeliveryOrderList(statementDeliveryOrderSelectRQ, pagination);
    }

    /**
     * @Description 更新销售发货单状态
     * @Param id
     * @Param state
     * @Date 2019/5/30 15:23
     * @Author xin.huang
     * @Return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<Integer> updateStateStatementDelivery(AuthPlatformUserInfo simpleUserInfo, Integer id, Integer state) {
        ErpRepositoryReceipt repositoryReceipt = repositoryReceiptMapper.selectOne(new ErpRepositoryReceipt()
                .setId(id).setDeleted(Status.FALSE.getKey()));
        if (Objects.isNull(repositoryReceipt)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.PURCHASE_GOODS_ORDER_NOT_EXIST);
        }
        repositoryReceiptMapper.update(new ErpRepositoryReceipt().setState(state)
                        .setUpdateUser(simpleUserInfo.getId()),
                new EntityWrapper<ErpRepositoryReceipt>().eq("id", id));
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);
    }

    @Override
    public ResponseResult<ErpInventoryFlowSearchDTO> searchInventoryFlowByCondition( ErpInventoryFlowSearchRQ rq, Page page,Integer companyId) {

        String[] t1 = {"公司","采购订单号","入库单号","入库日期","供应商","产品","收货数量","仓库","状态"};
        String[] t2 = {"公司","料批","出库单号","产品","仓库","出库日期","炉号","班次","出库数量","状态"};
        String[] t3 = {"公司","料批","入库单号","入库日期","仓库","炉号","班次","入库数量","状态"};
        String[] t4 = {"公司","销售订单号","出库单号","出库日期","客户","仓库","产品","发货数量","状态"};
        Integer type = rq.getType();
        BigDecimal zero4Big = new BigDecimal(0);
        Pagination pagination= PageUtils.transFromPage(page);
        ErpInventoryFlowSearchDTO dto = new ErpInventoryFlowSearchDTO();

        List<Integer> typeList = Lists.newArrayList(0,1,2,3);
        if(!typeList.contains(type)){
            log.info("type类型不匹配，type为：{}"+type);
            dto.setTitle(t1).setSum(zero4Big).setDtoList(Lists.newArrayList());
            return  ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,dto,PageUtils.transToPage(pagination));
        }
        List<AuthEnterpriseFlatDTO> enterpriseFlatDTOS = authEnterpriseFeignClient.getEnterpriseFlatByCompanyId(companyId).getObject();
        if (CollectionUtils.isEmpty(enterpriseFlatDTOS)) {
            log.error("未查询到当前用户企业信息！ 类：{} 方法：{}", "ErpRepositoryReceiptServiceImpl", "searchInventoryFlowByCondition");
            return ResponseResult.buildResponseResult(ResCodeEnum.GET_ENTERPRISE_USER_RELATION_FAILED,dto,PageUtils.transToPage(pagination));
        }
        List<Integer> ids = enterpriseFlatDTOS.stream().map(a -> a.getValue()).collect(Collectors.toList());
        List<ErpInventoryFlowDTO> list= Lists.newArrayList();
        if(!CollectionUtils.isEmpty(ids)) {
           rq.setList(ids);
            String startTime = rq.getStartTime();
            if (!StringUtils.isEmpty(startTime)) {
                rq.setStartTime(startTime + " 00:00:00");
            }
            String endTime = rq.getEndTime();
            if (!StringUtils.isEmpty(endTime)) {
                rq.setEndTime(endTime + " 23:59:59");
            }

           list = baseMapper.searchInventoryFlowByCondition(pagination, rq);
        }
        switch (type){
            case 0:
                dto.setTitle(t1);
                dto.setSum(list.stream().map(ErpInventoryFlowDTO::getReceiptNum).reduce(BigDecimal::add).orElse(zero4Big));
                dto.setDtoList(list);
                break;
            case 1:
                dto.setTitle(t2);
                dto.setSum(list.stream().map(ErpInventoryFlowDTO::getOutOfStockNum).reduce(BigDecimal::add).orElse(zero4Big));
                dto.setDtoList(list);
                break;
                case 2:
                dto.setTitle(t3);
                dto.setSum(list.stream().map(ErpInventoryFlowDTO::getWarehousingNum).reduce(BigDecimal::add).orElse(zero4Big));
                dto.setDtoList(list);
                break;
                case 3:
                dto.setTitle(t4);
                dto.setSum(list.stream().map(ErpInventoryFlowDTO::getSendNum).reduce(BigDecimal::add).orElse(zero4Big));
                dto.setDtoList(list);
                break;
            default:
                break;

        }
        return  ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,dto,PageUtils.transToPage(pagination));
    }


    /**
     * 保存原料入库主单
     * @param userInfo
     * @param rq
     * @return
     */

    @Transactional(rollbackFor = Exception.class)
    @Override
    public Integer saveRepositoryReceiptRawIn(AuthPlatformUserInfo userInfo, RepositoryReceiptRawInRQ rq) {
        Date time= new Date();
        Integer orgId = userInfo.getOrgId();
        Integer userId = userInfo.getId();


        ErpRepositoryReceipt order = BeanUtils.copyProperties(rq, ErpRepositoryReceipt.class);
        Integer relatedOrderId = rq.getRelatedOrderId();
        // 查询源采购订单
        ErpPurchaseOrder erpPurchaseOrder = erpPurchaseOrderService.selectById(relatedOrderId);
        if(ObjectUtils.isEmpty(erpPurchaseOrder)){
            log.error("保存原料入库失败，没有查到相关的源采购订单信息 调用{}的{}方法出错", "ErpRepositoryReceiptServiceImpl", "saveRepositoryReceiptRawIn()");
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_REPOSITORY_RECEIPT_RAW_IN_SAVE_FAILED_NO_PU_ORDER);
        }
        Integer companyId = erpPurchaseOrder.getCompany();
        if(ObjectUtils.isEmpty(companyId)){
            log.error("保存原料入库失败，没有查到相关的源采购订单中 的公司信息 调用{}的{}方法出错", "ErpRepositoryReceiptServiceImpl", "saveRepositoryReceiptRawIn()");
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_REPOSITORY_RECEIPT_RAW_IN_SAVE_FAILED_NO_COMPANY_ID);
        }

        String code = rq.getCode();
        List<ErpRepositoryReceipt> existCode = selectList(new EntityWrapper<ErpRepositoryReceipt>().eq("code", code).eq("org_id", companyId).eq("deleted", 0).eq("type","material_stock"));
        // 新增校验编号重复
        if (ObjectUtils.isEmpty(rq.getId()) && !CollectionUtils.isEmpty(existCode)) {
            log.error("单号编码重复，编码为:" + code);
            log.error("保存原料入库失败，单号编码重复，调用{}的{}方法出错", "ErpRepositoryReceiptServiceImpl", "saveRepositoryReceiptRawIn()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.ERP_REPOSITORY_RECEIPT_RAW_IN_SAVE_FAILED_CODE_RE);
        }
        // 修改校验编号重复
        if(!ObjectUtils.isEmpty(rq.getId()) && !CollectionUtils.isEmpty(existCode) && !existCode.get(0).getId().equals(rq.getId())){
                log.error("单号编码重复，编码为:" + code);
                log.error("保存原料入库失败，单号编码重复，调用{}的{}方法出错", "ErpRepositoryReceiptServiceImpl", "saveRepositoryReceiptRawIn()");
                throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.ERP_REPOSITORY_RECEIPT_RAW_IN_SAVE_FAILED_CODE_RE);

        }

        // 将源采购订单的公司id写入原料入库的公司id
        order.setOrgId(companyId);
        // 设置业务类型 为原料入库
        order.setType(EnumBusinessType.MATERIAL_STOCK.getCode());

        if(ObjectUtils.isEmpty(rq.getId())){
            order.setCreateTime(time).setCreateUser(userId).setEnterpriseId(orgId).setState(0);
        }else {
            order.setUpdateUser(userId).setUpdateTime(time).setState(0);
        }

        if(!insertOrUpdate(order)){
            log.error("保存原料入库失败，调用{}的{}方法出错", "ErpRepositoryReceiptServiceImpl", "saveRepositoryReceiptRawIn()");
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_REPOSITORY_RECEIPT_RAW_IN_SAVE_FAILED);
        }
        return order.getId();


    }


    /**
     * 修改原料入库确认状态
     * @param userInfo
     * @param id
     * @param state
     */

    @Transactional(rollbackFor = Exception.class)
    @Override
    public void updateRepositoryReceiptRawInState(AuthPlatformUserInfo userInfo, Integer id, Integer state) {
        ErpRepositoryReceipt exist = selectOne(new EntityWrapper<ErpRepositoryReceipt>().eq("id", id).eq("deleted", 0));
        if(ObjectUtils.isEmpty(exist)){
            log.info("根据id修改原料入库状态，没有找到相关数据，id为："+id);
            return;
        }
        Integer userId = userInfo.getId();
        Date time= new Date();
        ErpRepositoryReceipt order = new ErpRepositoryReceipt().setId(id).setState(state).setUpdateUser(userId).setUpdateTime(time);
        if(!updateById(order)){
            log.error("修改原料入库状态失败，调用{}的{}方法出错", "ErpRepositoryReceiptServiceImpl", "updateRepositoryReceiptRawInState()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.ERP_REPOSITORY_RECEIPT_RAW_IN_UPDATE_FAILED);
        }

    }

    /**
     * 删除原料入库单
     * @param userInfo
     * @param id
     */

    @Transactional(rollbackFor = Exception.class)
    @Override
    public void deleteRepositoryReceiptRawInById(AuthPlatformUserInfo userInfo, Integer id) {

        ErpRepositoryReceipt exist = selectOne(new EntityWrapper<ErpRepositoryReceipt>().eq("id", id).eq("deleted", 0));
        if(ObjectUtils.isEmpty(exist)){
            log.info("删除原料入库单时，根据id没有找到相关记录，id为:"+id);
            return;
        }

        // 删除主单
        if(!updateById(new ErpRepositoryReceipt().setId(id).setDeleted(1))){
            log.error("删除原料入库失败，调用{}的{}方法出错", "ErpRepositoryReceiptServiceImpl", "deleteRepositoryReceiptRawInById()");
            throw new BusinessException(ResCodeEnum.DELETE_FAILED, ExceptionMessageEnum.ERP_REPOSITORY_RECEIPT_RAW_IN_DELETE_FAILED);
        }

        // 根据主单id查询所有详情
        List<ErpRepoReceiptDetail> details = repoReceiptDetailService.selectList(new EntityWrapper<ErpRepoReceiptDetail>().eq("receipt_id", id).eq("deleted", 0));
        if(CollectionUtils.isEmpty(details)){
            log.info("删除原料入库单时，根据id没有找到相关明细单记录，id为:"+id);
            return;
        }

        // 删除详情
        // 获取详情ids集合
        List<Integer> detailIds = details.stream().map(ErpRepoReceiptDetail::getId).collect(Collectors.toList());
        // 依次删除详情
        for (Integer i : detailIds) {
            repoReceiptDetailService.deleteRepoReceiptDetailRawIn(userInfo,i);
        }



    }

    /**
     * 根据id查询 原料入库信息
     * @param id
     * @return
     */
    @Override
    public ErpRepositoryReceiptRawInDTO getRepositoryReceiptRawInById(Integer id) {
        ErpRepositoryReceiptRawInDTO dto = new ErpRepositoryReceiptRawInDTO();


        // 查询主表信息
        ErpRepositoryReceipt one = selectOne(new EntityWrapper<ErpRepositoryReceipt>().eq("id", id).eq("deleted", 0));
        if(ObjectUtils.isEmpty(one)){
            log.info("根据id查看原料入库,没有找到相关数据，id为："+id);
            return  dto;
        }

        one.setRelatedOrder(commonMapper.getPurchaseOrderCodeById(one.getRelatedOrderId()));

         dto = BeanUtils.copyProperties(one, ErpRepositoryReceiptRawInDTO.class);
        // 查询详情表信息
        List<ErpRepoReceiptDetailRawInDTO> detailRawInDTOList = repoReceiptDetailService.getRepoReceiptDetailRawInList(id);

        for (ErpRepoReceiptDetailRawInDTO d : detailRawInDTOList) {
            Integer productId = d.getProductId();
            ErpProduct p = productService.selectById(productId);
            if(!ObjectUtils.isEmpty(p)){
                d.setProductName(p.getName()).setUnit(p.getUnit());
            }

            d.setRepositoryName(commonMapper.getRepositoryNameById(d.getRepositoryId()));
            d.setTestCode(commonMapper.getTestCodeById(d.getTestId()));
            d.setResult(commonMapper.getTestResultById(d.getTestId()));

        }


        dto.setDetailRawInDTOList(detailRawInDTOList);

        return dto;
    }

    /**
     * 保存成品出库信息
     * @param userInfo
     * @param rq
     * @return
     */

    @Transactional(rollbackFor = Exception.class)
    @Override
    public Integer saveRepositoryReceiptProductOut(AuthPlatformUserInfo userInfo, ErpRepositoryReceiptProductOutRQ rq) {
        Date time= new Date();
        Integer orgId = userInfo.getOrgId();
        Integer userId = userInfo.getId();
        ErpRepositoryReceipt order = BeanUtils.copyProperties(rq, ErpRepositoryReceipt.class);
        Integer relatedOrderId = rq.getRelatedOrderId();
        // 查询源销售订单
        ErpSaleOrder erpSaleOrder = erpSaleOrderService.selectById(relatedOrderId);
        if(ObjectUtils.isEmpty(erpSaleOrder)){
            log.error("保存成品出库失败，没有查到相关的源销售订单信息 调用{}的{}方法出错", "ErpRepositoryReceiptServiceImpl", "saveRepositoryReceiptProductOut()");
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_REPOSITORY_RECEIPT_PRODUCT_OUT_SAVE_FAILED_NO_SALE_ORDER);
        }
        Integer companyId = erpSaleOrder.getCompany();
        if(ObjectUtils.isEmpty(companyId)){
            log.error("保存成品出库失败，没有查到相关的源销售订单中 的公司信息 调用{}的{}方法出错", "ErpRepositoryReceiptServiceImpl", "saveRepositoryReceiptProductOut()");
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_REPOSITORY_RECEIPT_PRODUCT_OUT_SAVE_FAILED_NO_COMPANY_ID);
        }

        String code = rq.getCode();
//        Integer companyId = rq.getCompanyId();
        List<ErpRepositoryReceipt> existCode = selectList(new EntityWrapper<ErpRepositoryReceipt>().eq("code", code).eq("org_id", companyId).eq("deleted", 0).eq("type","product_delivery"));
        // 新增校验编号重复
        if (ObjectUtils.isEmpty(rq.getId()) && !CollectionUtils.isEmpty(existCode)) {
            log.error("单号编码重复，编码为{}" + code);
            log.error("保存成品出库失败，单号编码重复，调用{}的{}方法出错", "ErpRepositoryReceiptServiceImpl", "saveRepositoryReceiptProductOut()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.ERP_REPOSITORY_RECEIPT_PRODUCT_OUT_SAVE_FAILED_CODE_RE);
        }
        // 修改校验编号重复
        if(!ObjectUtils.isEmpty(rq.getId()) && !CollectionUtils.isEmpty(existCode) && !existCode.get(0).getId().equals(rq.getId())){
            log.error("单号编码重复，编码为{}" + code);
            log.error("保存原料入库失败，单号编码重复，调用{}的{}方法出错", "ErpRepositoryReceiptServiceImpl", "saveRepositoryReceiptRawIn()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.ERP_REPOSITORY_RECEIPT_RAW_IN_SAVE_FAILED_CODE_RE);

        }


        // 将源销售订单的公司id写入成品出库的公司id
        order.setOrgId(companyId);
        // 设置业务类型 为成品出库
        order.setType(EnumBusinessType.PRODUCT_DELIVERY.getCode());

        if(ObjectUtils.isEmpty(rq.getId())){
            order.setCreateTime(time).setCreateUser(userId).setEnterpriseId(orgId).setState(0);
        }else {
            order.setUpdateUser(userId).setUpdateTime(time).setState(0);
        }

        if(!insertOrUpdate(order)){
            log.error("保存成品出库失败，调用{}的{}方法出错", "ErpRepositoryReceiptServiceImpl", "saveRepositoryReceiptProductOut()");
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_REPOSITORY_RECEIPT_PRODUCT_OUT_SAVE_FAILED);
        }
        return order.getId();

    }

    /**
     * 修改成品出库状态
     * @param userInfo
     * @param id
     * @param state
     */

    @Transactional(rollbackFor = Exception.class)
    @Override
    public void updateRepositoryReceiptProductOutState(AuthPlatformUserInfo userInfo, Integer id, Integer state) {
        ErpRepositoryReceipt exist = selectOne(new EntityWrapper<ErpRepositoryReceipt>().eq("id", id).eq("deleted", 0));
        if(ObjectUtils.isEmpty(exist)){
            log.info("根据id修改成品出库状态，没有找到相关数据，id为："+id);
            return;
        }

        Integer userId = userInfo.getId();
        Date time= new Date();
        ErpRepositoryReceipt order = new ErpRepositoryReceipt().setId(id).setState(state).setUpdateUser(userId).setUpdateTime(time);
        if(!updateById(order)){
            log.error("修改原料入库状态失败，调用{}的{}方法出错", "ErpRepositoryReceiptServiceImpl", "updateRepositoryReceiptProductOutState()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.ERP_REPOSITORY_RECEIPT_RAW_IN_UPDATE_FAILED);
        }

    }

    /**
     * 删除成品出库单
     * @param userInfo
     * @param id
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public void deleteRepositoryReceiptProductOutById(AuthPlatformUserInfo userInfo, Integer id) {
        ErpRepositoryReceipt exist = selectOne(new EntityWrapper<ErpRepositoryReceipt>().eq("id", id).eq("deleted", 0));
        if(ObjectUtils.isEmpty(exist)){
            log.info("删除成品出库单时，根据id没有找到相关记录，id为:"+id);
            return;
        }

        // 删除主单
        if(!updateById(new ErpRepositoryReceipt().setId(id).setDeleted(1))){
            log.error("删除原料入库失败，调用{}的{}方法出错", "ErpRepositoryReceiptServiceImpl", "deleteRepositoryReceiptRawInById()");
            throw new BusinessException(ResCodeEnum.DELETE_FAILED, ExceptionMessageEnum.ERP_REPOSITORY_RECEIPT_PRODUCT_OUT_DELETE_FAILED);
        }

        // 根据主单id查询所有详情
        List<ErpRepoReceiptDetail> details = repoReceiptDetailService.selectList(new EntityWrapper<ErpRepoReceiptDetail>().eq("receipt_id", id).eq("deleted", 0));
        if(CollectionUtils.isEmpty(details)){
            log.info("删除成品出库单时，根据id没有找到相关明细单记录，id为:"+id);
            return;
        }

        // 删除详情
        // 获取详情ids集合
        List<Integer> detailIds = details.stream().map(ErpRepoReceiptDetail::getId).collect(Collectors.toList());
        // 依次删除详情
        for (Integer i : detailIds) {
            repoReceiptDetailService.deleteRepoReceiptDetailProductOutById(userInfo,i);
        }

        //修改销售订单发货状态
        erpSaleOrderService.updateById(new ErpSaleOrder().setId(exist.getRelatedOrderId()).setDeliveryState(EnumErpSaleOrderStatus.DeliveryType.N0T_DELIVERY.getKey()));

    }

    /**
     * 查询成品出库信息
     * @param id
     * @return
     */
    @Override
    public ErpRepositoryReceiptProductOutDTO getRepositoryReceiptProductOutById(Integer id) {
        ErpRepositoryReceiptProductOutDTO dto = new ErpRepositoryReceiptProductOutDTO();

        // 查询主表信息
        ErpRepositoryReceipt one = selectOne(new EntityWrapper<ErpRepositoryReceipt>().eq("id", id).eq("deleted", 0));
        if(ObjectUtils.isEmpty(one)){
            log.info("根据id查看成品出库,没有找到相关数据，id为："+id);
            return dto;
        }
        one.setRelatedOrder(commonMapper.getSaleOrderCodeById(one.getRelatedOrderId()));



         dto = BeanUtils.copyProperties(one, ErpRepositoryReceiptProductOutDTO.class);
        // 查询详情表信息
        List<ErpRepoReceiptDetailProductOutDTO> detailProductOutDTOList = repoReceiptDetailService.getRepoReceiptDetailProductOutList(id);
        for (ErpRepoReceiptDetailProductOutDTO d : detailProductOutDTOList) {
            Integer productId = d.getProductId();
            ErpProduct p = productService.selectById(productId);
            if(!ObjectUtils.isEmpty(p)){
                d.setProductName(p.getName()).setUnit(p.getUnit());
            }

            d.setRepositoryName(commonMapper.getRepositoryNameById(d.getRepositoryId()));
            d.setTestCode(commonMapper.getTestCodeById(d.getTestId()));
            d.setResult(commonMapper.getTestResultById(d.getTestId()));

        }


        dto.setDetailProductOutDTOList(detailProductOutDTOList);

        return dto;
    }

    /**
     * 条件搜索原料入库列表
     * @param
     * @param rq
     * @param page
     * @param
     * @return
     */
    @Override
    public ResponseResult<List<ErpRepositoryReceiptRawInSearchDTO>> searchRepositoryReceiptRawInByCondition(ErpRepositoryReceiptRawInSearchRQ rq, Page page, Integer companyId) {
        Pagination pagination= PageUtils.transFromPage(page);
        List<ErpRepositoryReceiptRawInSearchDTO> dto = Lists.newArrayList();
        List<AuthEnterpriseFlatDTO> enterpriseFlatDTOS = authEnterpriseFeignClient.getEnterpriseFlatByCompanyId(companyId).getObject();
        if (CollectionUtils.isEmpty(enterpriseFlatDTOS)) {
            log.error("未查询到当前用户企业信息！ 类：{} 方法：{}", "ErpRepositoryReceiptServiceImpl", "searchRepositoryReceiptRawInByCondition");
            return ResponseResult.buildResponseResult(ResCodeEnum.GET_ENTERPRISE_USER_RELATION_FAILED, Lists.newArrayList(), PageUtils.transToPage(pagination));
        }
        List<Integer> ids = enterpriseFlatDTOS.stream().map(a -> a.getValue()).collect(Collectors.toList());
        if(CollectionUtils.isEmpty(ids)){
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,dto,PageUtils.transToPage(pagination));
        }
        rq.setList(ids);
        String startTime = rq.getStartTime();
        if(!StringUtils.isEmpty(startTime)) {
            rq.setStartTime(startTime + " 00:00:00");
        }
        String endTime = rq.getEndTime();
        if(!StringUtils.isEmpty(endTime)) {
           rq.setEndTime(endTime + " 23:59:59");
        }


        dto= baseMapper.searchRepositoryReceiptRawInByCondition(rq,pagination);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,dto,PageUtils.transToPage(pagination));

    }

    /**
     * 条件搜索成品出库列表
     * @param
     * @param rq
     * @param page
     * @param
     * @return
     */
    @Override
    public ResponseResult<List<ErpRepositoryReceiptProductOutSearchDTO>> searchRepositoryReceiptProductOutByCondition(ErpRepositoryReceiptProductOutSearchRQ rq, Page page, Integer companyId) {

        Pagination pagination= PageUtils.transFromPage(page);
        List<ErpRepositoryReceiptProductOutSearchDTO> dto = Lists.newArrayList();
        List<AuthEnterpriseFlatDTO> enterpriseFlatDTOS = authEnterpriseFeignClient.getEnterpriseFlatByCompanyId(companyId).getObject();
        if (CollectionUtils.isEmpty(enterpriseFlatDTOS)) {
            log.error("未查询到当前用户企业信息！ 类：{} 方法：{}", "ErpRepositoryReceiptServiceImpl", "searchRepositoryReceiptProductOutByCondition");
            return ResponseResult.buildResponseResult(ResCodeEnum.GET_ENTERPRISE_USER_RELATION_FAILED, Lists.newArrayList(), PageUtils.transToPage(pagination));
        }
        List<Integer> ids = enterpriseFlatDTOS.stream().map(a -> a.getValue()).collect(Collectors.toList());
        if(CollectionUtils.isEmpty(ids)){
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,dto,PageUtils.transToPage(pagination));
        }
        rq.setList(ids);
        String startTime = rq.getStartTime();
        if(!StringUtils.isEmpty(startTime)) {
            rq.setStartTime(startTime + " 00:00:00");
        }
        String endTime = rq.getEndTime();
        if(!StringUtils.isEmpty(endTime)) {
            rq.setEndTime(endTime + " 23:59:59");
        }


        dto= baseMapper.searchRepositoryReceiptProductOutByCondition(rq,pagination);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,dto,PageUtils.transToPage(pagination));

    }

    /**
     * @Description 根据销售订单id查看销售发货情况
     * @Param id
     * @Date 2019/6/12 18:24
     * @Author xin.huang
     * @Return
     */
    @Override
    public ResponseResult<List<RepoReceiptDetailDTO>> getSaleDeliveryInfo(Integer id) {
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, repoReceiptDetailMapper.getSaleDeliveryInfo(id));
    }
}
