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
import com.bee.platform.datadriver.dao.mapper.ErpMaterialBatchOrderMapper;
import com.bee.platform.datadriver.dto.*;
import com.bee.platform.datadriver.entity.ErpMaterialBatchOrder;
import com.bee.platform.datadriver.entity.ErpMaterialBatchOrderDetail;
import com.bee.platform.datadriver.entity.ErpProduct;
import com.bee.platform.datadriver.entity.ErpProductBatch;
import com.bee.platform.datadriver.rq.ErpMaterialBatchOrderRQ;
import com.bee.platform.datadriver.rq.ErpMaterialBatchSearchRQ;
import com.bee.platform.datadriver.service.*;
import com.bee.platform.user.authority.dto.AuthEnterpriseFeignDetailDTO;
import com.bee.platform.user.authority.dto.AuthEnterpriseFlatDTO;
import com.bee.platform.user.service.feign.AuthEnterpriseFeignClient;
import com.google.common.collect.Lists;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

/**
 * <p>
 * 料批主表 服务实现类
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-28
 */
@Slf4j
@Service
public class ErpMaterialBatchOrderServiceImpl extends ServiceImpl<ErpMaterialBatchOrderMapper, ErpMaterialBatchOrder> implements ErpMaterialBatchOrderService {

    @Autowired
    private ErpMaterialBatchOrderMapper erpMaterialBatchOrderMapper;
    @Autowired
    private ErpMaterialBatchOrderDetailService erpMaterialBatchOrderDetailService;
    @Autowired
    private AuthEnterpriseFeignClient authEnterpriseFeignClient;

    @Autowired
    private ErpProductService productService;

    @Autowired
    private ErpProductBatchService productBatchService;

    @Autowired
    private ErpOperationLogService erpOperationLogService;

    /**
     * 条件搜索料批列表
     *
     * @param
     * @param rq
     * @param page
     * @param
     * @return
     */
    @Override
    public ResponseResult<List<ErpMaterialBatchSearchListDTO>> searchMaterialBatchByCondition(ErpMaterialBatchSearchRQ rq, Page page, Integer companyId) {
        Pagination pagination = PageUtils.transFromPage(page);
//        List<ErpMaterialBatchSearchListDTO> dto = Lists.newArrayList();
//        List<AuthEnterpriseFlatDTO> enterpriseFlatDTOS = authEnterpriseFeignClient.getEnterpriseFlatByCompanyId(companyId).getObject();
//        if (org.springframework.util.CollectionUtils.isEmpty(enterpriseFlatDTOS)) {
//            log.error("未查询到当前用户企业信息！ 类：{} 方法：{}", "ErpMaterialBatchOrderServiceImpl", "searchMaterialBatchByCondition");
//            return ResponseResult.buildResponseResult(ResCodeEnum.GET_ENTERPRISE_USER_RELATION_FAILED, Lists.newArrayList(), PageUtils.transToPage(pagination));
//        }
//        List<Integer> ids = enterpriseFlatDTOS.stream().map(a -> a.getValue()).collect(Collectors.toList());
//        if (CollectionUtils.isEmpty(ids)) {
//            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto, PageUtils.transToPage(pagination));
//        }
//        rq.setList(ids);
        rq.setCompanyId(companyId);
        List<ErpMaterialBatchSearchListDTO>  dto = baseMapper.searchMaterialBatchByCondition(rq,pagination);
        if(CollectionUtils.isEmpty(dto)){
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto, PageUtils.transToPage(pagination));
        }
        List<Integer> cIds = dto.stream().map(o->o.getCompanyId()).distinct().collect(Collectors.toList());
        // 设置企业名称
        List<AuthEnterpriseFeignDetailDTO> companyList = authEnterpriseFeignClient.getEnterpriseMoreDetail(cIds).getObject();
        if(!CollectionUtils.isEmpty(companyList)){
            for (ErpMaterialBatchSearchListDTO d : dto) {
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
     * 修改料批状态
     *
     * @param userInfo
     * @param id
     * @param state
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public void updateMaterialBatchState(AuthPlatformUserInfo userInfo, Integer id, Integer state) {
        ErpMaterialBatchOrder exist = selectOne(new EntityWrapper<ErpMaterialBatchOrder>().eq("id", id).eq("deleted", 0));
        if(ObjectUtils.isEmpty(exist)){
            log.info("根据id修改料批状态，没有找到相关数据，id为："+id);
            return;
        }
        Integer userId = userInfo.getId();
        Date time = new Date();
        ErpMaterialBatchOrder order = new ErpMaterialBatchOrder().setId(id).setState(state).setModifierId(userId).setModifyTime(time);
        if (!updateById(order)) {
            log.error("修改料批状态失败，调用{}的{}方法出错", "ErpMaterialBatchOrderServiceImpl", "updateMaterialBatchState()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.ERP_MATERIAL_BATCH_ORDER_UPDATE_FAILED);
        }

    }

    @Transactional(rollbackFor = Exception.class)
    @Override
    public Integer saveMaterialBatchOrder(AuthPlatformUserInfo userInfo, ErpMaterialBatchOrderRQ rq) {
        Date time = new Date();
        Integer orgId = userInfo.getOrgId();
        Integer userId = userInfo.getId();
        ErpMaterialBatchOrder order = BeanUtils.copyProperties(rq, ErpMaterialBatchOrder.class);
        String msg;
        if (ObjectUtils.isEmpty(rq.getId())) {
            msg="新增";
            order.setCreateTime(time).setCreatorId(userId).setCreatorEnterpriseId(orgId);
        } else {
            msg="编辑";
            order.setModifierId(userId).setModifyTime(time);
        }
        if (!insertOrUpdate(order)) {
            log.error("保存料批失败，调用{}的{}方法出错", "ErpMaterialBatchOrderServiceImpl", "saveMaterialBatchOrder()");
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_MATERIAL_BATCH_ORDER_SAVE_FAILED);
        }
        // 保存操作日志
        erpOperationLogService.saveLog(order.getCompanyId(),userInfo,order.getId(),"material_batch",msg);

        return order.getId();
    }


    @Transactional(rollbackFor = Exception.class)
    @Override
    public void deleteMaterialBatchById(AuthPlatformUserInfo userInfo, Integer id) {
        Date time = new Date();
        Integer userId = userInfo.getId();
        ErpMaterialBatchOrder exist = selectOne(new EntityWrapper<ErpMaterialBatchOrder>().eq("id", id).eq("deleted", 0));
        if(ObjectUtils.isEmpty(exist)){
            log.info("根据id删除料批，没有找到相应数据，id为："+id);
            return;
        }

        // 删除主单
        if (!updateById(new ErpMaterialBatchOrder().setId(id).setModifierId(userId).setModifyTime(time).setDeleted(1))) {
            log.error("删除料批失败，调用{}的{}方法出错", "ErpMaterialBatchOrderServiceImpl", "deleteMaterialBatchById()");
            throw new BusinessException(ResCodeEnum.DELETE_FAILED, ExceptionMessageEnum.ERP_MATERIAL_BATCH_ORDER_DELETE_FAILED);
        }
        List<ErpMaterialBatchOrderDetail> details = erpMaterialBatchOrderDetailService.selectList(new EntityWrapper<ErpMaterialBatchOrderDetail>().eq("material_batch_order_id", id).eq("deleted", 0));
        if (CollectionUtils.isEmpty(details)) {
            log.info("删除料批详情时，没有找到相关数据,主单id为："+id);
            return;
        }
        ErpMaterialBatchOrderDetail orderDetail = new ErpMaterialBatchOrderDetail().setDeleted(1).setModifyTime(time).setModifierId(userId);
        Wrapper<ErpMaterialBatchOrderDetail> wrapper = new EntityWrapper<ErpMaterialBatchOrderDetail>().eq("material_batch_order_id", id).eq("deleted", 0);

        // 删除详情
        if (!erpMaterialBatchOrderDetailService.update(orderDetail, wrapper)) {
            log.error("删除料批失败，调用{}的{}方法出错", "ErpMaterialBatchOrderServiceImpl", "deleteMaterialBatchById()");
            throw new BusinessException(ResCodeEnum.DELETE_FAILED, ExceptionMessageEnum.ERP_MATERIAL_BATCH_ORDER_DETAIL_DELETE_FAILED);
        }


    }

    /**
     * 根据id查询料批信息
     *
     * @param id
     * @return
     */
    @Override
    public ErpMaterialBatchOrderDTO getMaterialBatchById(Integer id) {
        ErpMaterialBatchOrderDTO dto = new ErpMaterialBatchOrderDTO();

        // 查询主单
        ErpMaterialBatchOrder order = selectOne(new EntityWrapper<ErpMaterialBatchOrder>().eq("id", id).eq("deleted", 0));
        if (ObjectUtils.isEmpty(order)) {
            log.info("根据id查询料批信息,没有找到相关数据，id为："+id);
            return dto;
        }
        dto = BeanUtils.copyProperties(order, ErpMaterialBatchOrderDTO.class);
        // 设置企业名称
        List<AuthEnterpriseFeignDetailDTO> companyList = authEnterpriseFeignClient.getEnterpriseMoreDetail(Lists.newArrayList(order.getCompanyId())).getObject();
        if(!CollectionUtils.isEmpty(companyList)){
            dto.setCompanyName(companyList.get(0).getName());
        }
        ErpProduct p1 = productService.selectById(order.getProductId());
        ErpProductBatch pb1 = productBatchService.selectById(order.getProductBatchId());
        if (!ObjectUtils.isEmpty(p1)) {
            dto.setProductName(p1.getName()).setUnit(p1.getUnit());
        }
        if (!ObjectUtils.isEmpty(p1) && !ObjectUtils.isEmpty(pb1)) {
            dto.setProductAndBatch(p1.getName()+"-"+pb1.getBatchName());
        }else if(!ObjectUtils.isEmpty(p1) && ObjectUtils.isEmpty(pb1)) {
            dto.setProductAndBatch(p1.getName());
        }

        // 查询详情单
        List<ErpMaterialBatchOrderDetail> details = erpMaterialBatchOrderDetailService.selectList(new EntityWrapper<ErpMaterialBatchOrderDetail>().eq("material_batch_order_id", id).eq("deleted", 0));
        List<ErpMaterialBatchOrderDetailDTO> detailDTOList = BeanUtils.assemble(ErpMaterialBatchOrderDetailDTO.class, details);
        for (ErpMaterialBatchOrderDetailDTO d : detailDTOList) {
            ErpProduct p2 = productService.selectById(d.getProductId());
            ErpProductBatch pb2 = productBatchService.selectById(d.getProductBatchId());
            if (!ObjectUtils.isEmpty(p2)) {
                d.setProductName(p2.getName()).setUnit(p2.getUnit());
            }
            if (!ObjectUtils.isEmpty(p2) && !ObjectUtils.isEmpty(pb2)) {
                d.setProductAndBatch(p2.getName()+"-"+pb2.getBatchName());
            }else if(!ObjectUtils.isEmpty(p2) && ObjectUtils.isEmpty(pb2)) {
                d.setProductAndBatch(p2.getName());
            }

        }

        dto.setDetailList(detailDTOList);

        return dto;
    }

    /**
     * 根据企业id和产品id查询料批
     */
    @Override
    public ResponseResult<List<ErpMaterialBatchOrder>> getMaterialBatch(Integer productId, Integer companyId) {
        List<ErpMaterialBatchOrder> list = erpMaterialBatchOrderMapper.selectList(new EntityWrapper<>(new ErpMaterialBatchOrder()
                .setProductId(productId).setCompanyId(companyId).setDeleted(0)));
        if (CollectionUtils.isEmpty(list)) {
            log.info("根据企业id和产品id查询料批，没有查到数据");
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,new ArrayList<>());
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, list);

    }


    /**
     * 根据登录企业查询料批列表
     *
     * @param userInfo
     * @return
     */
    @Override
    public List<ErpMaterialBatchListDTO> getMaterialBatchList(AuthPlatformUserInfo userInfo, String sysToken) {
        List<AuthEnterpriseFlatDTO> enterpriseFlatDTOS = authEnterpriseFeignClient.getEnterpriseFlatByUser(sysToken).getObject();
        if (org.springframework.util.CollectionUtils.isEmpty(enterpriseFlatDTOS)) {
            log.info("未查询到当前用户企业信息！ 类：{} 方法：{}", "ErpMaterialBatchOrderServiceImpl", "getMaterialBatchList");
            return Lists.newArrayList();
        }
        List<Integer> ids = enterpriseFlatDTOS.stream().map(AuthEnterpriseFlatDTO::getValue).collect(Collectors.toList());

        if (org.springframework.util.CollectionUtils.isEmpty(ids)) {
            return Lists.newArrayList();
        }

        List<ErpMaterialBatchListDTO> dto = erpMaterialBatchOrderMapper.getMaterialBatchList(ids);

        if(CollectionUtils.isEmpty(dto)){
            return dto;
        }
        List<Integer> cIds = dto.stream().map(o->o.getCompanyId()).distinct().collect(Collectors.toList());
        // 设置企业名称
        List<AuthEnterpriseFeignDetailDTO> companyList = authEnterpriseFeignClient.getEnterpriseMoreDetail(cIds).getObject();
        if(!CollectionUtils.isEmpty(companyList)){
            for (ErpMaterialBatchListDTO d : dto) {
                for (AuthEnterpriseFeignDetailDTO e : companyList) {
                    if(d.getCompanyId().equals(e.getId())){
                        d.setCompanyName(e.getName());
                    }
                }
            }
        }

        return dto;
    }

    /**
     * 根据公司id查询公司下的料批列表
     *
     * @param companyId
     * @return
     */
    @Override
    public List<ErpMaterialBatchListDTO> getMaterialBatchListByCompanyId(Integer companyId) {
        List<Integer> id = Lists.newArrayList(companyId);

        List<ErpMaterialBatchListDTO> dto = erpMaterialBatchOrderMapper.getMaterialBatchList(id);
        if(CollectionUtils.isEmpty(dto)){
            return dto;
        }
        List<Integer> cIds = dto.stream().map(o->o.getCompanyId()).distinct().collect(Collectors.toList());
        // 设置企业名称
        List<AuthEnterpriseFeignDetailDTO> companyList = authEnterpriseFeignClient.getEnterpriseMoreDetail(cIds).getObject();
        if(!CollectionUtils.isEmpty(companyList)){
            for (ErpMaterialBatchListDTO d : dto) {
                for (AuthEnterpriseFeignDetailDTO e : companyList) {
                    if(d.getCompanyId().equals(e.getId())){
                        d.setCompanyName(e.getName());
                    }
                }
            }
        }

        return dto;
    }

    /**
     * 根据料批id查询料批明细里的产品列表
     *
     * @param id
     * @return
     */
    @Override
    public List<ErpProductAndBatchListDTO> getMaterialBatchDetailProductList(Integer id) {
        return erpMaterialBatchOrderMapper.getMaterialBatchDetailProductList(id);
    }
}
