package com.bee.platform.datadriver.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.baomidou.mybatisplus.toolkit.CollectionUtils;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.service.JedisService;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.DateUtils;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.datadriver.dao.mapper.*;
import com.bee.platform.datadriver.dto.*;
import com.bee.platform.datadriver.entity.*;
import com.bee.platform.datadriver.enums.EnumReceiveType;
import com.bee.platform.datadriver.rq.*;
import com.bee.platform.datadriver.service.*;
import com.google.common.collect.Lists;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.stream.Collectors;

/**
 * <p>
 * 物流订单 服务实现类
 * </p>
 *
 * @author hongchuan.he123
 * @since 2019-07-24
 */
@Slf4j
@Service
public class ErpLogisticsOrdersServiceImpl extends ServiceImpl<ErpLogisticsOrdersMapper, ErpLogisticsOrders> implements ErpLogisticsOrdersService {

    @Autowired
    private ErpLogisticsOrdersMapper erpLogisticsOrdersMapper;

    @Autowired
    private ErpLogisticsOrdersDetailMapper erpLogisticsOrdersDetailMapper;

    @Autowired
    private CommonService commonService;

    @Autowired
    private ErpLogisticsStatusDetailService erpLogisticsStatusDetailService;

    @Autowired
    private JedisService jedisService;

    @Autowired
    private ErpLogisticsSettlementMapper erpLogisticsSettlementMapper;

    @Autowired
    private ErpLogisticsStatusDetailMapper erpLogisticsStatusDetailMapper;

    @Autowired
    private ErpLogisticsInvoiceDetailMapper erpLogisticsInvoiceDetailMapper;

    @Autowired
    private ErpLogisticsPaymentMapper erpLogisticsPaymentMapper;

    @Autowired
    private ErpLogisticsInvoiceMapper erpLogisticsInvoiceMapper;

    @Autowired
    private ErpLogisticsOrdersDetailService erpLogisticsOrdersDetailService;

    @Autowired
    private ErpLogisticsSettlementService erpLogisticsSettlementService;

    /**
     * 查询物流订单列表
     */
    @Override
    public ResponseResult<List<ErpLogisticsOrdersDTO>> query(Integer companyId, ErpLogisticsOrdersQueryRQ rq, Pagination pagination) {

//    	List<AuthEnterpriseFlatDTO> enterprises = authEnterpriseFeignClient
//                    .getEnterpriseFlatByCompanyId(companyId).getObject();
//            if (CollectionUtils.isNotEmpty(enterprises)) {
//                List<Integer> enterpriseIds = new ArrayList<Integer>();
//                enterprises.forEach(enterprise -> {
//                    enterpriseIds.add(enterprise.getValue());
//                });
//                rq.setEnterpriseIdList(enterpriseIds);
//            }
        if (Objects.nonNull(companyId)) {
            rq.setEnterpriseIdList(Arrays.asList(companyId));
        }
        List<ErpLogisticsOrdersDTO> list = erpLogisticsOrdersMapper.query(rq, pagination);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, list, PageUtils.transToPage(pagination));
    }


    /**
     * 编辑需要返回物流订单
     */
    @Override
    public ResponseResult<ErpLogisticsLogisticsTrackingDetailDTO> getTrackingDetail(Integer id) {
        ErpLogisticsLogisticsTrackingDetailDTO dto = new ErpLogisticsLogisticsTrackingDetailDTO();
        if (Objects.nonNull(id)) {
            ErpLogisticsOrders erpLogisticsOrders = erpLogisticsOrdersMapper.selectById(id);
            List<ErpLogisticsOrdersDetail> list = erpLogisticsOrdersDetailMapper.selectList(new EntityWrapper<>(new ErpLogisticsOrdersDetail().setOrderId(id)));
            BeanUtils.copyProperties(erpLogisticsOrders, dto);
            List<ErpLogisticsStatusDetailDTO> assemble = BeanUtils.assemble(ErpLogisticsStatusDetailDTO.class, list);
            if (CollectionUtils.isNotEmpty(assemble)) {
                dto.setList(assemble);
            }
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);

    }

    /**
     * 添加物流订单
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<?> addOrUpdate(AuthPlatformUserInfo userInfo, ErpLogisticsOrdersAddRQ rq) {
        Integer id = 0;
        // 新增物流订单
        if (Objects.isNull(rq.getId())) {
            ErpLogisticsOrders erpLogisticsOrders = BeanUtils.copyProperties(rq, ErpLogisticsOrders.class);
            // 自动生产物流订单号
            // 编码
            if (Objects.isNull(erpLogisticsOrders.getOrderNumber())) {
                erpLogisticsOrders.setOrderNumber(GenerateOrderId(userInfo.getOrgId()));
            }
            //校验订单号是否重复
            ErpLogisticsOrders logisticsOrder = this.selectOne(new EntityWrapper<>(new ErpLogisticsOrders()
                    .setCompanyId(userInfo.getOrgId())
                    .setOrderNumber(erpLogisticsOrders.getOrderNumber())));
            //校验订单号是否重复
            if (Objects.nonNull(logisticsOrder)) {
                return ResponseResult.buildResponseResult(ResCodeEnum.LOGISTICS_ORDER_NUMBER_NOT);
            }

            erpLogisticsOrders.setCreateUser(userInfo.getId()).setCreateTime(new Date())
                    .setCompanyId(userInfo.getOrgId()).setCompanyName(userInfo.getOrg_name());

            if (erpLogisticsOrdersMapper.insert(erpLogisticsOrders.setPayStatus(2)) < 0) {
                return ResponseResult.buildResponseResult(ResCodeEnum.ADD_FAILED);
            }
            id = erpLogisticsOrders.getId();
            // 新增物流订单详情
            List<ErpLogisticsOrdersDetailAddRQ> list = rq.getList();
            for (ErpLogisticsOrdersDetailAddRQ erpLogisticsOrdersDetailAddRQ : list) {
                ErpLogisticsOrdersDetail erpLogisticsOrdersDetail = BeanUtils.copyProperties(erpLogisticsOrdersDetailAddRQ,
                        ErpLogisticsOrdersDetail.class);
                String batchName = erpLogisticsOrdersDetail.getBatchName();
                if (batchName == null || "".equals(batchName)) {
                    erpLogisticsOrdersDetail.setBatchName(null);
                }
                erpLogisticsOrdersDetail.setOrderId(erpLogisticsOrders.getId()).setCreateTime(new Date())
                        .setCreateUser(userInfo.getId()).setDeleted(Status.FALSE.getKey());
                if (erpLogisticsOrdersDetailMapper.insert(erpLogisticsOrdersDetail) < 0) {
                    return ResponseResult.buildResponseResult(ResCodeEnum.ADD_FAILED);
                }
            }
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);
        }
        // 编辑物流订单
        else {
            ErpLogisticsOrders erpLogisticsOrders = erpLogisticsOrdersMapper.selectById(rq.getId());
            String orderNumber = rq.getOrderNumber();
            if (Objects.isNull(orderNumber)) {
                return ResponseResult.buildResponseResult(ResCodeEnum.ORDER_NUMBER_NOT);
            }
            if (Objects.nonNull(erpLogisticsOrders)) {
                erpLogisticsOrders = BeanUtils.copyProperties(rq, erpLogisticsOrders);
                erpLogisticsOrders.setUpdateUser(userInfo.getId()).setUpdateTime(new Date())
                        .setCompanyId(userInfo.getOrgId());
                if (Objects.isNull(rq.getOrderNumber())) {
                    erpLogisticsOrders.setOrderNumber(GenerateOrderId(userInfo.getOrgId()));
                }
                //校验订单号是否重复
                ErpLogisticsOrders logisticsOrder = this.selectOne(new EntityWrapper<>(new ErpLogisticsOrders()
                        .setCompanyId(userInfo.getOrgId())
                        .setOrderNumber(erpLogisticsOrders.getOrderNumber())));
                //校验订单号是否重复
                if (Objects.nonNull(logisticsOrder) && !logisticsOrder.getId().equals(rq.getId())) {
                    return ResponseResult.buildResponseResult(ResCodeEnum.LOGISTICS_ORDER_NUMBER_NOT);
                }

                if (erpLogisticsOrdersMapper.updateById(erpLogisticsOrders) < 1) {
                    return ResponseResult.buildResponseResult(ResCodeEnum.ADD_FAILED);
                }
                id = erpLogisticsOrders.getId();

                //物流订单详情跟新
                List<ErpLogisticsOrdersDetail> erpLogisticsOrdersDetailList = BeanUtils.assemble(ErpLogisticsOrdersDetail.class, rq.getList());
                for (ErpLogisticsOrdersDetail erpLogisticsOrdersDetail : erpLogisticsOrdersDetailList) {
                    erpLogisticsOrdersDetail.setCreateUser(userInfo.getId()).setCreateTime(new Date()).setDeleted(Status.FALSE.getKey()).setOrderId(erpLogisticsOrders.getId());
                }
                List<ErpLogisticsOrdersDetail> list = erpLogisticsOrdersDetailList.stream().filter(o -> o.getId() == null).collect(Collectors.toList());
                if (null != list && list.size() > 0) {
                    erpLogisticsOrdersDetailService.insertBatch(list);
                }
                erpLogisticsOrdersDetailList.removeAll(list);
                if (erpLogisticsOrdersDetailList != null && erpLogisticsOrdersDetailList.size() > 0) {
                    erpLogisticsOrdersDetailService.updateBatchById(erpLogisticsOrdersDetailList);
                }
            }
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);
    }

    /**
     * 删除物流订单明细
     */
    @Override
    public ResponseResult<?> deleteLogisticsOrdersDetail(AuthPlatformUserInfo userInfo, Integer id) {
        Integer updateById = erpLogisticsOrdersDetailMapper.updateById(new ErpLogisticsOrdersDetail().setId(id).setDeleted(Status.TRUE.getKey()));
        log.info("修改物流订单的明细=>updateById={}", updateById);
        if (updateById < 0) {
            return ResponseResult.buildResponseResult(ResCodeEnum.UPDATE_FAIL);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);
    }


    /**
     * 修改物流订单产品返回数据列表
     */
    @Override
    public ResponseResult<List<ErpLogisticsOrdersDetail>> queryOrders(Integer id) {
        if (Objects.nonNull(id)) {
            //物流订单明细集合
            List<ErpLogisticsOrdersDetail> selectList = erpLogisticsOrdersDetailMapper.selectList(new EntityWrapper<>(new ErpLogisticsOrdersDetail().setOrderId(id)));
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, selectList);
        }
        return null;

    }

    /**
     * 物流订单产品批次返回数据列表
     */
    @Override
    public ResponseResult<List<ErpLogisticsOrdersDetail>> queryBatchId(Integer id, Integer productId) {
        if (Objects.nonNull(id) && Objects.nonNull(productId)) {
            //物流订单明细集合
            List<ErpLogisticsOrdersDetail> selectList = erpLogisticsOrdersDetailMapper.selectList(new EntityWrapper<>(new ErpLogisticsOrdersDetail().setProductId(productId)
                    .setOrderId(id)
                    .setDeleted(Status.FALSE.getKey())));
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, selectList);
        }
        return null;

    }

    /**
     * 物流订单产品批次返回数据列表
     */
    @Override
    public ResponseResult<List<ErpLogisticsOrdersDetail>> queryBatchIdOrProductId(Integer batchId, Integer id, Integer productId) {
        if (Objects.nonNull(batchId) && Objects.nonNull(id) && Objects.nonNull(productId)) {
            //物流订单明细集合
            List<ErpLogisticsOrdersDetail> selectList = erpLogisticsOrdersDetailMapper.selectList(new EntityWrapper<>(new ErpLogisticsOrdersDetail()
                    .setProductId(productId)
                    .setOrderId(id)
                    .setBatchId(batchId)
                    .setDeleted(Status.FALSE.getKey())));
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, selectList);
        }
        return null;

    }

    /**
     * 添加物流订单状态返回数据
     */
    @Override
    public ResponseResult<List<ErpLogisticsOrdersDetailBatchIdDTO>> queryOrdersDetailBatchIdDTO(Integer id) {

        List<ErpLogisticsOrdersDetail> list = erpLogisticsOrdersDetailMapper.selectList(new EntityWrapper<>(new ErpLogisticsOrdersDetail().setOrderId(id).setDeleted(Status.FALSE.getKey())));
        Map<Integer, ErpLogisticsOrdersDetailBatchIdDTO> map = new HashMap<>();
        list.forEach(detail -> {
            Integer productId = detail.getProductId();
            Integer batchId = detail.getBatchId();

            ErpLogisticsDetailBatchIdDTO batchDTO = new ErpLogisticsDetailBatchIdDTO();
            batchDTO.setBatchId(batchId);
            batchDTO.setBatchName(detail.getBatchName());
            if (map.containsKey(productId)) {
                List<ErpLogisticsDetailBatchIdDTO> batchList = map.get(productId).getBatchList();
                if (CollectionUtils.isNotEmpty(batchList) && !batchList.contains(batchDTO)) {
                    batchList.add(batchDTO);
                }
            } else {
                ErpLogisticsOrdersDetailBatchIdDTO productDTO = new ErpLogisticsOrdersDetailBatchIdDTO().setProductId(productId)
                        .setProductName(detail.getProductName()).setUnit(detail.getUnit());
                List<ErpLogisticsDetailBatchIdDTO> batchList = Lists.newArrayList();
                batchList.add(batchDTO);
                productDTO.setBatchList(batchList);
                map.put(productId, productDTO);
            }
        });
        List<ErpLogisticsOrdersDetailBatchIdDTO> result = Lists.newArrayList();
        result.addAll(map.values());
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, result);
    }

    /**
     * 修改物流订单状态的时候返回数据列表
     */
    @Override
    public ResponseResult<List<ErpLogisticsOrdersDetailDTO>> queryOrdersDetail(Integer id) {
        ArrayList<ErpLogisticsOrdersDetailDTO> dto = new ArrayList<>();
        ErpLogisticsOrdersDetailDTO erpLogisticsOrdersDetailDTO = new ErpLogisticsOrdersDetailDTO();
        if (Objects.nonNull(id)) {
            //物流订单明细集合
            List<ErpLogisticsOrdersDetail> listOrdersDetail = erpLogisticsOrdersDetailMapper.selectList(new EntityWrapper<>(new ErpLogisticsOrdersDetail().setOrderId(id)));
            //获取批次id集合
            List<Integer> list = listOrdersDetail.stream().map(a -> a.getBatchId()).collect(Collectors.toList());
            for (Integer batchId : list) {
                ErpLogisticsOrdersDetail erpLogisticsOrdersDetail = erpLogisticsOrdersDetailMapper.selectOne(new ErpLogisticsOrdersDetail().setBatchId(batchId).setDeleted(Status.FALSE.getKey()));
                BeanUtils.copyProperties(erpLogisticsOrdersDetail, ErpLogisticsOrdersDetailDTO.class);
                dto.add(erpLogisticsOrdersDetailDTO);
            }
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);

    }

    /**
     * 修改物流订单状态
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult update(AuthPlatformUserInfo userInfo, ErpLogisticsStatusRQ rq) {

        ErpLogisticsOrders erpLogisticsOrders = erpLogisticsOrdersMapper.selectById(rq.getId());
        //物流订单明细集合
        List<ErpLogisticsStatusDetail> erpLogisticsStatusDetailList = null;
        if (!org.springframework.util.CollectionUtils.isEmpty(rq.getList())) {
            erpLogisticsStatusDetailList = rq.getList().stream().map(o -> {
                ErpLogisticsStatusDetail dest = new ErpLogisticsStatusDetail();
                BeanUtils.copyProperties(o, dest);
                dest.setReceivingTime(DateUtils.parse(o.getReceivingTime(), DateUtils.Y_M_D));
                return dest;
            }).collect(Collectors.toList());
        }
        //Status=1时候在运输途中状态去新增物流明细 物流明细就只用展示（产品、车号、合同重量、计量单位）
        if (rq.getStatus().equals(Status.TRUE.getKey())) {
            if (Objects.nonNull(erpLogisticsOrders)) {
                //修改物流订单状态
                erpLogisticsOrders.setStatus(rq.getStatus())
                        .setEstimatedArrivalTime(rq.getEstimatedArrivalTime())
                        .setDeliveryTime(DateUtils.parse(rq.getDeliveryTime(), DateUtils.Y_M_D))
                        .setUpdateUser(userInfo.getId())
                        .setUpdateTime(new Date());
            }
            if (erpLogisticsOrdersMapper.updateById(erpLogisticsOrders) != 1) {
                return ResponseResult.buildResponseResult(ResCodeEnum.UPDATE_FAIL);
            }
            //修改物流订单状态详情
            if (erpLogisticsStatusDetailList != null) {

                erpLogisticsStatusDetailList.forEach(
                        a -> a.setCreateUser(userInfo.getId())
                                .setCreateTime(new Date())
                                .setDeleted(Status.FALSE.getKey())
                                .setOrderId(erpLogisticsOrders.getId()));
                List<ErpLogisticsStatusDetail> newList = erpLogisticsStatusDetailList.stream().filter(o -> o.getId() == null).collect(Collectors.toList());
                erpLogisticsStatusDetailList.removeAll(newList);
                if (null != newList && newList.size() > 0) {
                    for (ErpLogisticsStatusDetail detail : newList) {
                        if (detail.getBatchName() == null || "".equals(detail.getBatchName())) {
                            detail.setBatchName(null);
                        }
                    }
                    erpLogisticsStatusDetailService.insertBatch(newList);
                }
                if (null != erpLogisticsStatusDetailList && erpLogisticsStatusDetailList.size() > 0) {
                    erpLogisticsStatusDetailService.updateBatchById(erpLogisticsStatusDetailList);
                }
            }
        }
        //在已到货的时候只能修改皮重、毛重、净重量
        else if (rq.getStatus().equals(2)) {
            if (Objects.nonNull(erpLogisticsOrders)) {
                //修改物流订单状态
                erpLogisticsOrders.setStatus(rq.getStatus())
                        .setUpdateUser(userInfo.getId())
                        .setUpdateTime(new Date());
            }
            if (erpLogisticsOrdersMapper.updateById(erpLogisticsOrders) != 1) {
                return ResponseResult.buildResponseResult(ResCodeEnum.UPDATE_FAIL);
            }
            //修改物流订单详情

            erpLogisticsStatusDetailList.forEach(
                    a -> a.setUpdateUser(userInfo.getId()));
            for (ErpLogisticsStatusDetail erpLogisticsStatusDetail : erpLogisticsStatusDetailList) {
                String batchName = erpLogisticsStatusDetail.getBatchName();
                if (batchName == null || "".equals(batchName)) {
                    erpLogisticsStatusDetail.setBatchName(null);
                }
                if (erpLogisticsStatusDetailMapper.updateById(erpLogisticsStatusDetail) != 1) {
                    log.error("新增物流状态详情失败，类：{} 方法：{}", "ErpLogisticsStatusDetailServiceImpl");
                }
            }
        }
        //status=0时候就是没有发货状态
        else if (rq.getStatus().equals(Status.FALSE.getKey())) {
            if (Objects.nonNull(erpLogisticsOrders)) {
                //修改物流订单状态
                erpLogisticsOrders.setStatus(rq.getStatus())
                        .setUpdateUser(userInfo.getId())
                        .setUpdateTime(new Date());
            }
            if (erpLogisticsOrdersMapper.updateById(erpLogisticsOrders) != 1) {
                return ResponseResult.buildResponseResult(ResCodeEnum.UPDATE_FAIL);
            }
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, erpLogisticsOrders.getId());
    }

    /**
     * 逻辑删除物流订单状态明细
     */
    @Override
    public ResponseResult<?> delete(AuthPlatformUserInfo userInfo, Integer id) {
        Integer updateById = erpLogisticsStatusDetailMapper.updateById(new ErpLogisticsStatusDetail().setId(id)
                .setDeleted(Status.TRUE.getKey()));
        log.info("修改物流订单的明细=>updateById={}", updateById);
        if (updateById < 0) {
            return ResponseResult.buildResponseResult(ResCodeEnum.UPDATE_FAIL);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,id);

    }

    /**
     * 添加物流结算
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult addSettlement(AuthPlatformUserInfo userInfo, ErpLogisticsSettlementRQ rq) {
        if (Objects.nonNull(rq.getOrderId())) {
            //删除该订单下的结算单
            erpLogisticsSettlementMapper.delete(new EntityWrapper<>(new ErpLogisticsSettlement().setOrderId(rq.getOrderId())));

            //新增结算单
            if (CollectionUtils.isNotEmpty(rq.getSettlementDetails())) {
                List<ErpLogisticsSettlementDetailRQ> settlementDetails = rq.getSettlementDetails();
                List<ErpLogisticsSettlement> statementList = BeanUtils.assemble(ErpLogisticsSettlement.class, settlementDetails);
                statementList.forEach(stmt -> {
                    stmt.setOrderId(rq.getOrderId())
                            .setCreateTime(new Date())
                            .setUpdateTime(new Date())
                            .setCreateUser(userInfo.getId())
                            .setUpdateUser(userInfo.getId());
                });
                erpLogisticsSettlementService.insertBatch(statementList);
            }
        }

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, rq.getOrderId());

    }

    /**
     * 查询物流跟踪列表
     */
    @Override
    public ResponseResult<List<ErpLogisticsLogisticsTrackingDTO>> queryStatus(AuthPlatformUserInfo userInfo, ErpLogisticsLogisticsTrackingRQ rq, Pagination pagination) {
//		List<AuthEnterpriseFlatDTO> enterprises = authEnterpriseFeignClient
//                 .getEnterpriseFlatByCompanyId(orgId).getObject();
//         if (CollectionUtils.isNotEmpty(enterprises)) {
//             List<Integer> enterpriseIds = new ArrayList<Integer>();
//             enterprises.forEach(enterprise -> {
//                 enterpriseIds.add(enterprise.getValue());
//             });
//             rq.setEnterpriseIdList(enterpriseIds);
//         }
//
        if (rq.getEnterpriseIdList() == null) {
            rq.setEnterpriseIdList(Lists.newArrayList());
        }
        rq.getEnterpriseIdList().add(userInfo.getOrgId());
        List<ErpLogisticsLogisticsTrackingDTO> list = erpLogisticsOrdersMapper.queryStatus(rq, pagination);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, list, PageUtils.transToPage(pagination));

    }

    /**
     * 修改发货明细
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<?> updateDetails(AuthPlatformUserInfo userInfo, ErpLogisticsShippingDetailsRQ rq) {
        List<ErpLogisticsStatusDetail> erpLogisticsStatusDetailList = BeanUtils.assemble(ErpLogisticsStatusDetail.class, rq.getList());
        erpLogisticsStatusDetailList.forEach(
                a -> a.setUpdateUser(userInfo.getId())
                        .setUpdateTime(new Date()));
        for (ErpLogisticsStatusDetail erpLogisticsStatusDetail : erpLogisticsStatusDetailList) {
            if (erpLogisticsStatusDetail.getBatchName() == null || "".equals(erpLogisticsStatusDetail.getBatchName())) {
                erpLogisticsStatusDetail.setBatchName(null);
            }
            if (erpLogisticsStatusDetailMapper.updateById(erpLogisticsStatusDetail) != 1) {
                log.error("新增物流状态详情失败，类：{} 方法：{}", "ErpLogisticsStatusDetailServiceImpl");
            }
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, rq.getId());
    }

    /**
     * 查询物流跟踪详情
     */
    @Override
    public ResponseResult<ErpLogisticsLogisticsTrackingDetailDTO> get(AuthPlatformUserInfo userInfo, Integer id) {
        ErpLogisticsLogisticsTrackingDetailDTO erpLogisticsLogisticsTrackingDetailDTO = new ErpLogisticsLogisticsTrackingDetailDTO();
        ErpLogisticsOrders erpLogisticsOrders = erpLogisticsOrdersMapper.selectById(id);
        if (!Objects.isNull(erpLogisticsOrders)) {
            BeanUtils.copyProperties(erpLogisticsOrders, ErpLogisticsLogisticsTrackingDetailDTO.class);
        }
        List<ErpLogisticsOrdersDetail> list = erpLogisticsOrdersDetailMapper.selectList(new EntityWrapper<>(new ErpLogisticsOrdersDetail().setOrderId(id)));
        List<ErpLogisticsStatusDetailDTO> assemble = BeanUtils.assemble(ErpLogisticsStatusDetailDTO.class, list);
        if (CollectionUtils.isNotEmpty(assemble)) {
            erpLogisticsLogisticsTrackingDetailDTO.setList(assemble);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, erpLogisticsLogisticsTrackingDetailDTO);

    }


    /**
     * 编辑物流跟踪
     */
    @Override
    public ResponseResult<?> updateLogisticsTracking(AuthPlatformUserInfo userInfo, ErpUpdateLogisticsTrackingRQ rq) {
        Integer id = rq.getId();
        if (null != id) {
            ErpLogisticsOrders erpLogisticsOrders = erpLogisticsOrdersMapper.selectById(id);
            if (Objects.nonNull(erpLogisticsOrders)) {
                List<ErpLogisticsStatusDetail> erpLogisticsStatusDetailList = BeanUtils.assemble(ErpLogisticsStatusDetail.class, rq.getList());
                erpLogisticsStatusDetailList.forEach(
                        a -> a.setUpdateUser(userInfo.getId())
                                .setUpdateTime(new Date()));
                //过滤订单状态明细没有id的
                List<ErpLogisticsStatusDetail> newList = erpLogisticsStatusDetailList.stream().filter(o -> o.getId() == null).collect(Collectors.toList());
                erpLogisticsStatusDetailList.removeAll(newList);
                if (null != newList && newList.size() > 0) {
                    for (ErpLogisticsStatusDetail detail : newList) {
                        if (detail.getBatchName() == null || "".equals(detail.getBatchName())) {
                            detail.setBatchName(null);
                        }
                    }
                    erpLogisticsStatusDetailService.insertBatch(newList);
                }
                if (null != erpLogisticsStatusDetailList && erpLogisticsStatusDetailList.size() > 0) {
                    erpLogisticsStatusDetailService.updateBatchById(erpLogisticsStatusDetailList);
                }
            }

        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);
    }

    /**
     * 查询物流订单详情
     */
    @Override
    public ResponseResult<ErpLogisticsLogisticsTrackingDetailDTO> getLogisticsOrders(AuthPlatformUserInfo userInfo, Integer id) {
        BigDecimal sum = BigDecimal.ZERO;
        ErpLogisticsOrders erpLogisticsOrders = erpLogisticsOrdersMapper.selectById(id);
        if (Objects.isNull(erpLogisticsOrders)) {
            log.error("查询物流发票失败 ");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_NOT_FOUND);
        }

        ErpLogisticsLogisticsTrackingDetailDTO dto = BeanUtils.copyProperties(erpLogisticsOrders, ErpLogisticsLogisticsTrackingDetailDTO.class);
        //物流订单详情
        List<ErpLogisticsOrdersDetail> list = erpLogisticsOrdersDetailMapper.selectList(new EntityWrapper<>(new ErpLogisticsOrdersDetail()
                .setOrderId(id).setDeleted(Status.FALSE.getKey())));
        for (ErpLogisticsOrdersDetail erpLogisticsOrdersDetail : list) {
            BigDecimal n = erpLogisticsOrdersDetail.getNumber();
            sum = sum.add(n == null ? BigDecimal.ZERO : n);
        }

        dto.setList(BeanUtils.assemble(ErpLogisticsStatusDetailDTO.class, list)).setSumContractWeight(sum);

        //收货状态明细
        dto.setListStatus(BeanUtils.assemble(ErpLogisticsStatusDetailPlusDTO.class,
                erpLogisticsStatusDetailMapper.selectList(new EntityWrapper<>(new ErpLogisticsStatusDetail()
                        .setOrderId(id).setDeleted(Status.FALSE.getKey())))));

        //物流订单结算
        dto.setListSettlement(BeanUtils.assemble(ErpLogisticsSettlementDTO.class,
                erpLogisticsSettlementMapper.selectList(new EntityWrapper<>(new ErpLogisticsSettlement().setOrderId(id)))));

        //物流发票明细
        ErpLogisticsInvoice erpLogisticsInvoice = erpLogisticsInvoiceMapper.selectOne(new ErpLogisticsInvoice().setOrderId(id));
        if (Objects.nonNull(erpLogisticsInvoice)) {
            SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
            String format = sdf.format(erpLogisticsInvoice.getInvoiceTime());
            //添加物流发票中的 开票时间和备注
            dto.setInvoiceNumber(erpLogisticsInvoice.getInvoiceNumber())
                    .setInvoiceId(erpLogisticsInvoice.getId()).setInvoiceTime(format)
                    .setRemarksInvoice(erpLogisticsInvoice.getRemarks());
        }


        dto.setListInvoiceDetail(BeanUtils.assemble(ErpLogisticsInvoiceDetailDTO.class,
                erpLogisticsInvoiceDetailMapper.selectList(new EntityWrapper<>(new ErpLogisticsInvoiceDetail()
                        .setOrderId(id).setDeleted(Status.FALSE.getKey())))));

        //物流付款
        dto.setListPayment(BeanUtils.assemble(ErpLogisticsPaymentDTO.class,
                erpLogisticsPaymentMapper.selectList(new EntityWrapper<>(new ErpLogisticsPayment().setOrderId(id).setDeleted(Status.FALSE.getKey())))));

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
    }

    /**
     * 订单状态统计数
     *
     * @param orgId
     * @return
     */
    @Override
    public ResponseResult<OrderStatusCountDTO> countErpLogisticsOrdersStatu(Integer orgId) {
        Integer sum = 0;
        OrderStatusCountDTO countDTO = new OrderStatusCountDTO();
        List<ErpLogisticsOrders> total = erpLogisticsOrdersMapper
                .selectList(new EntityWrapper<ErpLogisticsOrders>().eq("company_id", orgId));
        if (CollectionUtils.isEmpty(total)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,
                    countDTO.setAccountCount(0).setCollectionCount(0).setDeliveryCount(0));
        }
        // 待发货
        Integer deliveryCount = erpLogisticsOrdersMapper.selectCount(new EntityWrapper<ErpLogisticsOrders>()
                .eq("status", EnumReceiveType.RECEIVE_NOT.getKey()).and().eq("company_id", orgId));
        // 在途中
        Integer count = erpLogisticsOrdersMapper.selectCount(new EntityWrapper<ErpLogisticsOrders>()
                .eq("company_id", orgId).and().eq("status", Status.TRUE.getKey()));

        List<ErpLogisticsOrders> list = erpLogisticsOrdersMapper.selectList(new EntityWrapper<>(new ErpLogisticsOrders().setCompanyId(orgId).setStatus(Status.TRUE.getKey())));
        //获取该企业下所有的所有订单id
        List<Integer> collect = list.stream().map(a -> a.getId()).collect(Collectors.toList());
        for (Integer id : collect) {
            // 已收货
            Integer selectCount2 = erpLogisticsStatusDetailMapper.selectCount(new EntityWrapper<ErpLogisticsStatusDetail>()
                    .eq("deleted", Status.FALSE.getKey()).and().eq("order_id", id));
            //.eq("status", EnumReceiveType.RECEIVED_PART.getKey()).and()
            sum = sum + selectCount2;
        }

        // 待结算
		/*List<ErpLogisticsSettlement> accounts = erpLogisticsSettlementMapper
				.selectList(new EntityWrapper<ErpLogisticsSettlement>()
						.in("order_id", total.stream().map(a -> a.getId()).collect(Collectors.toList()))
						.groupBy("order_id"));*/
        // 待付款
		/*List<ErpLogisticsPayment> pays = erpLogisticsPaymentMapper.selectList(new EntityWrapper<ErpLogisticsPayment>()
				.eq("status", EnumPayType.PAY_ALREADY.getKey()).and().eq("company_id", orgId)
				.in("order_id", total.stream().map(a -> a.getId()).collect(Collectors.toList())).groupBy("order_id"));*/

        Integer pay = erpLogisticsOrdersMapper.selectCount(new EntityWrapper<>(new ErpLogisticsOrders()
                .setCompanyId(orgId).setPayStatus(2)));
        countDTO.setDeliveryCount(deliveryCount);
        countDTO.setAccountCount(count);
        countDTO.setCollectionCount(pay);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, countDTO);
    }

    private String GenerateOrderId(Integer orgId) {
        String redisKey = "SIWL" + orgId;
        SimpleDateFormat simpleDateFormat = new SimpleDateFormat("yyyyMMdd");
        String format = simpleDateFormat.format(new Date());
        int orderCode = jedisService.incrOne(redisKey);

        return "WL" + format + commonService.getCode(orderCode + "", 3);
    }

}
