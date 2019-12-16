package com.bee.platform.datadriver.service.impl;

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
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.datadriver.dao.mapper.CommonMapper;
import com.bee.platform.datadriver.dao.mapper.ErpWarehousingOrderMapper;
import com.bee.platform.datadriver.dto.*;
import com.bee.platform.datadriver.entity.ErpProduct;
import com.bee.platform.datadriver.entity.ErpProductBatch;
import com.bee.platform.datadriver.entity.ErpStock;
import com.bee.platform.datadriver.entity.ErpWarehousingOrder;
import com.bee.platform.datadriver.rq.ErpGetOneWarehousingOrderRQ;
import com.bee.platform.datadriver.rq.ErpWarehousingOrderRQ;
import com.bee.platform.datadriver.rq.ErpWarehousingOrderSearchRQ;
import com.bee.platform.datadriver.service.*;
import com.bee.platform.user.authority.dto.AuthEnterpriseFeignDetailDTO;
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
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

/**
 * <p>
 * 成品入库主表 服务实现类
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-28
 */

@Slf4j
@Service
public class ErpWarehousingOrderServiceImpl extends ServiceImpl<ErpWarehousingOrderMapper, ErpWarehousingOrder> implements ErpWarehousingOrderService {

    @Autowired
    private ErpStockService erpStockService;

    @Autowired
    private ErpProductService erpProductService;

    @Autowired
    private CommonMapper commonMapper;

    @Autowired
    private ErpProductService productService;

    @Autowired
    private ErpProductBatchService productBatchService;

    @Autowired
    private ErpOperationLogService erpOperationLogService;
    @Autowired
    private AuthEnterpriseFeignClient authEnterpriseFeignClient;

    @Override
    public ResponseResult<List<ErpWarehousingOrderSearchListDTO>> searchWarehousingOrderByCondition(ErpWarehousingOrderSearchRQ rq, Page page, Integer companyId) {
        Pagination pagination = PageUtils.transFromPage(page);
//        List<ErpWarehousingOrderSearchListDTO> dto = Lists.newArrayList();
//        List<AuthEnterpriseFlatDTO> enterpriseFlatDTOS = authEnterpriseFeignClient.getEnterpriseFlatByCompanyId(companyId).getObject();
//        if (org.springframework.util.CollectionUtils.isEmpty(enterpriseFlatDTOS)) {
//            log.error("未查询到当前用户企业信息！ 类：{} 方法：{}", "ErpWarehousingOrderServiceImpl", "searchWarehousingOrderByCondition");
//            return ResponseResult.buildResponseResult(ResCodeEnum.GET_ENTERPRISE_USER_RELATION_FAILED, Lists.newArrayList(), PageUtils.transToPage(pagination));
//        }
//        List<Integer> ids = enterpriseFlatDTOS.stream().map(a -> a.getValue()).collect(Collectors.toList());
//        if (CollectionUtils.isEmpty(ids)) {
//            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto, PageUtils.transToPage(pagination));
//        }
//        rq.setList(ids);
        rq.setCompanyId(companyId);
        String startTime = rq.getStartTime();
        if (!StringUtils.isEmpty(startTime)) {
            rq.setStartTime(startTime + " 00:00:00");
        }
        String endTime = rq.getEndTime();
        if (!StringUtils.isEmpty(endTime)) {
            rq.setEndTime(endTime + " 23:59:59");
        }

        List<ErpWarehousingOrderSearchListDTO> dto = baseMapper.searchWarehousingOrderByCondition(rq,pagination);
        if(CollectionUtils.isEmpty(dto)){
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto, PageUtils.transToPage(pagination));
        }
        List<Integer> cIds = dto.stream().map(o->o.getCompanyId()).distinct().collect(Collectors.toList());
        // 设置企业名称
        List<AuthEnterpriseFeignDetailDTO> companyList = authEnterpriseFeignClient.getEnterpriseMoreDetail(cIds).getObject();
        if(!CollectionUtils.isEmpty(companyList)){
            for (ErpWarehousingOrderSearchListDTO d : dto) {
                for (AuthEnterpriseFeignDetailDTO e : companyList) {
                    if(d.getCompanyId().equals(e.getId())){
                        d.setCompanyName(e.getName());
                    }
                }
            }
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto, PageUtils.transToPage(pagination));

    }

    /**
     * 根据id查看成品入库
     * @param id
     * @return
     */
    @Override
    public ErpWarehousingOrderDTO getWarehousingOrderById(Integer id) {
        ErpWarehousingOrderDTO dto = new ErpWarehousingOrderDTO();
        // 查询详情
        ErpWarehousingOrder m = selectOne(new EntityWrapper<ErpWarehousingOrder>().eq("id", id).eq("deleted", 0));
        if (ObjectUtils.isEmpty(m)) {
            log.info("根据id查看成品入库,没有找到相关数据，id为："+id);
            return dto;
        }
        dto = BeanUtils.copyProperties(m, ErpWarehousingOrderDTO.class);

        dto.setMaterialBatchName(commonMapper.getMaterialBatchNameById(m.getMaterialBatchId()));
        List<AuthEnterpriseFeignDetailDTO> companyList = authEnterpriseFeignClient.getEnterpriseMoreDetail(Lists.newArrayList(m.getCompanyId())).getObject();
        if(!CollectionUtils.isEmpty(companyList)){
            dto.setCompanyName(companyList.get(0).getName());
        }

        dto.setFurnaceNumber(commonMapper.getFurnaceNameById(m.getFurnaceId()));

        ErpProduct p = productService.selectById(m.getProductId());
        ErpProductBatch pb = productBatchService.selectById(m.getProductBatchId());
        if (!ObjectUtils.isEmpty(p)) {
            dto.setProductName(p.getName()).setUnit(p.getUnit());
        }
        if (!ObjectUtils.isEmpty(p) && !ObjectUtils.isEmpty(pb)) {
            dto.setProductAndBatch(p.getName()+"-"+pb.getBatchName());
        }else if(!ObjectUtils.isEmpty(p) && ObjectUtils.isEmpty(pb)) {
            dto.setProductAndBatch(p.getName());
        }

        dto.setTestReportCode(commonMapper.getTestCodeById(m.getTestReportId()));
        dto.setStoreHouseName(commonMapper.getRepositoryNameById(m.getRepositoryId()));
        return dto;

    }

    @Transactional(rollbackFor = Exception.class)
    @Override
    public void deleteWarehousingOrderById(AuthPlatformUserInfo userInfo, Integer id) {
        Date time = new Date();
        Integer userId = userInfo.getId();
        ErpWarehousingOrder exist = selectOne(new EntityWrapper<ErpWarehousingOrder>().eq("id", id).eq("deleted", 0));
        if (ObjectUtils.isEmpty(exist)) {
            log.info("没有找到相关成品入库");
            return;
        }
        BigDecimal oldAmount = exist.getAmount();
        Integer companyId = exist.getCompanyId();
        Integer productId = exist.getProductId();
        Integer productBatchId = exist.getProductBatchId();
        Integer repositoryId = exist.getRepositoryId();
        Wrapper<ErpStock> wrapper = new EntityWrapper<ErpStock>().eq("org_id", companyId)
                .eq("product_id", productId).eq("repository_id", repositoryId).eq("deleted", 0);
        if(!ObjectUtils.isEmpty(productBatchId)){
            wrapper.eq("product_batch_id", productBatchId);
        }else {
            wrapper.isNull("product_batch_id");
        }
        ErpStock existStock = erpStockService.selectOne(wrapper);
        if (!ObjectUtils.isEmpty(existStock)) {
            BigDecimal oldInStockNum = existStock.getInStockNum();
            BigDecimal oldStockNum = existStock.getStockNum();
            existStock.setInStockNum(oldInStockNum.subtract(oldAmount));
            existStock.setStockNum(oldStockNum.subtract(oldAmount));

            if (!erpStockService.updateById(existStock)) {
                log.error("删除成品入库失败，调整库存数量失败,调用{}的{}方法出错", "ErpWarehousingOrderServiceImpl", "deleteWarehousingOrderById()");
                throw new BusinessException(ResCodeEnum.DELETE_FAILED, ExceptionMessageEnum.ERP_WAREHOUSING_ORDER_DELETE_FAILED_UPDATE_STOCK_FAILED);

            }
        } else {

            log.info("删除成品入库时，调整库存量，没有找到相关记录");
        }

        // 删除成品入库
        if (!updateById(new ErpWarehousingOrder().setId(id).setModifierId(userId).setModifyTime(time).setDeleted(1))) {
            log.error("删除成品入库失败，调用{}的{}方法出错", "ErpWarehousingOrderServiceImpl", "deleteWarehousingOrderById()");
            throw new BusinessException(ResCodeEnum.DELETE_FAILED, ExceptionMessageEnum.ERP_WAREHOUSING_ORDER_DELETE_FAILED);
        }


    }


    @Transactional(rollbackFor = Exception.class)
    @Override
    public void updateWarehousingOrderState(AuthPlatformUserInfo userInfo, Integer id, Integer state) {
        ErpWarehousingOrder exist = selectOne(new EntityWrapper<ErpWarehousingOrder>().eq("id", id).eq("deleted", 0));
        if(ObjectUtils.isEmpty(exist)){
            log.info("根据id修改成品入库状态，没有找到相关数据，id为："+id);
            return;
        }
        Integer userId = userInfo.getId();
        Date time = new Date();
        ErpWarehousingOrder order = new ErpWarehousingOrder().setId(id).setState(state).setModifierId(userId).setModifyTime(time);
        if (!updateById(order)) {
            log.error("修改成品入库状态失败，调用{}的{}方法出错", "ErpWarehousingOrderServiceImpl", "updateWarehousingOrderState()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.ERP_WAREHOUSING_ORDER_UPDATE_FAILED);
        }


    }

    /**
     * 保存成品入库
     *
     * @param userInfo
     * @param rq
     * @return
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public Integer saveWarehousingOrder(AuthPlatformUserInfo userInfo, ErpWarehousingOrderRQ rq) {

        Date time = new Date();
        Integer orgId = userInfo.getOrgId();
        Integer userId = userInfo.getId();
        Integer rqId = rq.getId();
//        Integer testReportId = rq.getTestReportId();

        String code = rq.getCode();
        Integer companyId = rq.getCompanyId();
        List<ErpWarehousingOrder> existCode = selectList(new EntityWrapper<ErpWarehousingOrder>().eq("code", code).eq("company_id", companyId).eq("deleted", 0));
        // 新增校验编号重复
        if (ObjectUtils.isEmpty(rq.getId()) && !CollectionUtils.isEmpty(existCode)) {
            log.info("单号编码重复，编码为:" + code);
            log.info("保存成品入库失败，单号编码重复，调用{}的{}方法出错", "ErpWarehousingOrderServiceImpl", "saveWarehousingOrder()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.ERP_WAREHOUSING_ORDER_SAVE_FAILED_CODE_RE);
        }
        // 修改校验编号重复
        if(!ObjectUtils.isEmpty(rq.getId()) && !CollectionUtils.isEmpty(existCode) && !existCode.get(0).getId().equals(rq.getId())){
            log.info("单号编码重复，编码为:" + code);
            log.info("保存成品入库失败，单号编码重复，调用{}的{}方法出错", "ErpWarehousingOrderServiceImpl", "saveWarehousingOrder()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.ERP_WAREHOUSING_ORDER_SAVE_FAILED_CODE_RE);

        }

        ErpWarehousingOrder order = BeanUtils.copyProperties(rq, ErpWarehousingOrder.class);
        String msg;
        if (ObjectUtils.isEmpty(rqId)) {
            msg="新增";
            // 新增记录
            order.setCreateTime(time).setCreatorId(userId).setCreatorEnterpriseId(orgId).setState(0);
        } else {
            msg="编辑";
            // 编辑记录
            order.setModifierId(userId).setModifyTime(time).setState(0);
        }

        // 保存成品入库数量到库存表
        Integer productId = rq.getProductId();
        Integer productBatchId = rq.getProductBatchId();
        Integer repositoryId = rq.getRepositoryId();
        ErpProductCategoryNameDTO category = erpProductService.getProductCategoryName(productId);
        String categoryName = "";
        Integer productCategoryId=null;
        if (!ObjectUtils.isEmpty(category)) {
            categoryName = category.getCategoryName();
            productCategoryId = category.getProductCategoryId();
        }
        BigDecimal amount = rq.getAmount();

        Wrapper<ErpStock> wrapper = new EntityWrapper<ErpStock>().eq("org_id", companyId)
                .eq("product_id", productId).eq("repository_id", repositoryId).eq("deleted", 0);
        if(!ObjectUtils.isEmpty(productBatchId)){
            wrapper.eq("product_batch_id", productBatchId);
        }else {
            wrapper.isNull("product_batch_id");
        }
        ErpStock existStock = erpStockService.selectOne(wrapper);
        // 如果不存在则创建新记录
        if (ObjectUtils.isEmpty(existStock)) {
            ErpStock stock = new ErpStock()
                    .setOrgId(companyId)
                    .setRepositoryId(repositoryId)
                    .setProductId(productId)
                    .setProductBatchId(productBatchId)
                    .setProductCategoryId(productCategoryId)
                    .setProductCategory(categoryName)
                    .setUnit(rq.getUnit())
                    .setInStockNum(amount)
                    .setStockNum(amount)
                    .setTestReportId(rq.getTestReportId());
            if (!erpStockService.insert(stock)) {
                log.error("保存成品入库失败,保存库存信息失败，调用{}的{}方法出错", "ErpWarehousingOrderServiceImpl", "saveWarehousingOrder()");
                throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_WAREHOUSING_ORDER_SAVE_FAILED_UPDATE_STOCK_FAILED);

            }
            // 如果存在记录则修改
        } else {
            // 如果是新添加的
            if (ObjectUtils.isEmpty(rqId)) {
                BigDecimal addInStock = amount.add(existStock.getInStockNum());
                BigDecimal addStock = amount.add(existStock.getStockNum());
                existStock.setInStockNum(addInStock);
                existStock.setStockNum(addStock);
            } else {

                // 如果是编辑 则取出历史的  old+new-old
                ErpWarehousingOrder exist = selectById(rqId);
                if (!ObjectUtils.isEmpty(exist)) {
                    BigDecimal oldAmount = exist.getAmount();
                    BigDecimal oldInStockNum = existStock.getInStockNum();
                    BigDecimal oldStockNum = existStock.getStockNum();
                    existStock.setInStockNum(oldInStockNum.add(amount).subtract(oldAmount));
                    existStock.setStockNum(oldStockNum.add(amount).subtract(oldAmount));
                } else {

                    log.info("编辑成品入库时，调整库存量，没有找到相关记录");
                }


            }

            if (!erpStockService.updateById(existStock)) {
                log.error("保存成品入库失败,保存库存信息失败，调用{}的{}方法出错", "ErpWarehousingOrderServiceImpl", "saveWarehousingOrder()");
                throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_WAREHOUSING_ORDER_SAVE_FAILED_UPDATE_STOCK_FAILED);
            }
        }


        if (!insertOrUpdate(order)) {
            log.error("保存成品入库失败，调用{}的{}方法出错", "ErpWarehousingOrderServiceImpl", "saveWarehousingOrder()");
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_WAREHOUSING_ORDER_SAVE_FAILED);
        }

//        if(!ObjectUtils.isEmpty(testReportId)){
//            ErpTestReport erpTestReport = erpTestReportService.selectById(testReportId);
//            if(!ObjectUtils.isEmpty(erpTestReport)){
//                erpTestReport.setOrderNo(order.getId());
//                if(!erpTestReportService.updateById(erpTestReport)){
//                    log.error("保存成品入库失败，调用{}的{}方法出错", "ErpWarehousingOrderServiceImpl", "saveWarehousingOrder()");
//                    throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_WAREHOUSING_ORDER_SAVE_FAILED_UPDATE_ORDER_ID_TO_TEST_REPORT_FAILED);
//
//                }
//            }else {
//                log.info("根据化验单id，没有查询到化验单记录，不更新单号到化验单");
//            }
//        }else {
//            log.info("没有化验单id，不更新单号到化验单");
//        }

        // 保存操作日志
        erpOperationLogService.saveLog(order.getCompanyId(),userInfo,order.getId(),"product_stock",msg);

        return order.getId();
    }

    /**
     * 根据当前登录用户查询其成品入库订单列表
     *
     * @param  companyId
     * @return
     */
    @Override
    public List<ErpWarehousingListDTO> getUserWarehousingOrderList(Integer companyId) {

        List<ErpWarehousingOrder> list = selectList(new EntityWrapper<ErpWarehousingOrder>().eq("deleted", 0).eq("company_id", companyId));
        List<ErpWarehousingListDTO> dto = BeanUtils.assemble(ErpWarehousingListDTO.class, list);
        if(!CollectionUtils.isEmpty(dto)){
            for (ErpWarehousingListDTO d : dto) {
                d.setContractNo(d.getCode());
            }
        }

        return dto;
    }

    /**
     * 根据料批id查询产成品id名称和单位
     *
     * @param id
     * @return
     */
    @Override
    public ErpMaterialBatchProductDTO getProductByMaterialBatchId(Integer id) {
        return baseMapper.getProductByMaterialBatchId(id);
    }

    /**
     * 根据条件查询一条成品入库单号
     * @param rq
     * @return
     */
    @Override
    public ErpGetOneWarehousingDTO getOneWarehousingOrder(ErpGetOneWarehousingOrderRQ rq) {

        ErpWarehousingOrder order = selectOne(new EntityWrapper<ErpWarehousingOrder>().eq("deleted", 0)
                .eq("company_id", rq.getCompanyId())
                .eq("furnace_id", rq.getFurnaceId())
                .eq("product_id", rq.getProductId())
                .eq("classes", rq.getClasses())
                .eq("warehousing_time", rq.getWarehousingTime())
        );
        if(ObjectUtils.isEmpty(order)){
            return new ErpGetOneWarehousingDTO();
        }
        return new ErpGetOneWarehousingDTO().setWarehousingOrderId(order.getId()).setCode(order.getCode()).setContractNo(order.getCode());
    }

    /**
     * 根据化验单id查询产品等信息
     * @param id
     * @return
     */
    @Override
    public ProductAndOtherDTO getProductAndOtherByTestReportId(Integer id) {

        return baseMapper.getProductAndOtherByTestReportId(id);
    }
}
