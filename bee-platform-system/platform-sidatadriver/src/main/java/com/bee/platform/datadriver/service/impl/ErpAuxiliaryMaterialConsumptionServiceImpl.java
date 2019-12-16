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
import com.bee.platform.datadriver.dao.mapper.ErpAuxiliaryMaterialConsumptionMapper;
import com.bee.platform.datadriver.dto.ErpAuxiliaryMaterialConsumptionDTO;
import com.bee.platform.datadriver.entity.ErpAuxiliaryMaterialConsumption;
import com.bee.platform.datadriver.rq.ErpAuxiliaryMaterialConsumptionRQ;
import com.bee.platform.datadriver.rq.ErpAuxiliaryMaterialConsumptionSearchRQ;
import com.bee.platform.datadriver.service.ErpAuxiliaryMaterialConsumptionService;
import com.bee.platform.datadriver.service.ErpOperationLogService;
import com.bee.platform.datadriver.service.ErpProductService;
import com.bee.platform.user.authority.dto.AuthEnterpriseFeignDetailDTO;
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

import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

/**
 * <p>
 * 辅材消耗表 服务实现类
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-28
 */

@Slf4j
@Service
public class ErpAuxiliaryMaterialConsumptionServiceImpl extends ServiceImpl<ErpAuxiliaryMaterialConsumptionMapper, ErpAuxiliaryMaterialConsumption> implements ErpAuxiliaryMaterialConsumptionService {

    @Autowired
    private AuthEnterpriseFeignClient authEnterpriseFeignClient;

    @Autowired
    private CommonMapper commonMapper;

    @Autowired
    private ErpOperationLogService erpOperationLogService;
    /**
     * 条件搜索辅材消耗
     *
     * @param
     * @param rq
     * @param page
     * @return
     */
    @Override
    public ResponseResult<List<ErpAuxiliaryMaterialConsumptionDTO>> searchAuxiliaryMaterialConsumptionByCondition( ErpAuxiliaryMaterialConsumptionSearchRQ rq, Page page, Integer companyId) {

        Pagination pagination = PageUtils.transFromPage(page);
//        List<ErpAuxiliaryMaterialConsumptionDTO> dto = Lists.newArrayList();
//        List<AuthEnterpriseFlatDTO> enterpriseFlatDTOS = authEnterpriseFeignClient.getEnterpriseFlatByCompanyId(companyId).getObject();
//
//        if (org.springframework.util.CollectionUtils.isEmpty(enterpriseFlatDTOS)) {
//            log.error("未查询到当前用户企业信息！ 类：{} 方法：{}", "ErpAuxiliaryMaterialConsumptionServiceImpl", "searchAuxiliaryMaterialConsumptionByCondition");
//            return ResponseResult.buildResponseResult(ResCodeEnum.GET_ENTERPRISE_USER_RELATION_FAILED, Lists.newArrayList(), PageUtils.transToPage(pagination));
//        }
//        List<Integer> ids = enterpriseFlatDTOS.stream().map(a -> a.getValue()).collect(Collectors.toList());
//        if (CollectionUtils.isEmpty(ids)) {
//            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto, PageUtils.transToPage(pagination));
//        }
//
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
        List<ErpAuxiliaryMaterialConsumptionDTO> dto = baseMapper.searchAuxiliaryMaterialConsumptionByCondition(rq,pagination);
        if(CollectionUtils.isEmpty(dto)){
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto, PageUtils.transToPage(pagination));
        }
        List<Integer> cIds = dto.stream().map(ErpAuxiliaryMaterialConsumptionDTO::getCompanyId).distinct().collect(Collectors.toList());
        // 设置企业名称
        List<AuthEnterpriseFeignDetailDTO> companyList = authEnterpriseFeignClient.getEnterpriseMoreDetail(cIds).getObject();
        if(!CollectionUtils.isEmpty(companyList)){
            for (ErpAuxiliaryMaterialConsumptionDTO d : dto) {
                for (AuthEnterpriseFeignDetailDTO e : companyList) {
                    if(d.getCompanyId().equals(e.getId())){
                        d.setCompanyName(e.getName());
                    }
                }
            }
        }

        for (ErpAuxiliaryMaterialConsumptionDTO d : dto) {
            d.setOutput(commonMapper.getOutPutFromWHO(d.getCompanyId(), d.getFurnaceId(), d.getConsumptionDate()));
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto, PageUtils.transToPage(pagination));
    }

    /**
     * 保存辅材消耗
     *
     * @param userInfo
     * @param rq
     * @return
     */

    @Transactional(rollbackFor = Exception.class)
    @Override
    public Integer saveAuxiliaryMaterialConsumption(AuthPlatformUserInfo userInfo, ErpAuxiliaryMaterialConsumptionRQ rq) {
        if (ObjectUtils.isEmpty(rq.getEnvironmentalMoney()) && ObjectUtils.isEmpty(rq.getOverhaulMoney()) && ObjectUtils.isEmpty(rq.getProductionMoney()) && ObjectUtils.isEmpty(rq.getSecurityMoney())) {
            log.info("保存辅材消耗状态失败，四个金额 至少填一个  调用{}的{}方法出错", "ErpAuxiliaryMaterialConsumptionServiceImpl", "saveAuxiliaryMaterialConsumption()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.ERP_AUXILIARY_MATERIAL_CONSUMPTION_SAVE_FAILED_NO_MONEY);
        }
        Date time = new Date();
        Integer orgId = userInfo.getOrgId();
        Integer userId = userInfo.getId();
        ErpAuxiliaryMaterialConsumption order = BeanUtils.copyProperties(rq, ErpAuxiliaryMaterialConsumption.class)
                .setCreatorId(userId).setCreatorEnterpriseId(orgId);
        String msg;
        if (ObjectUtils.isEmpty(rq.getId())) {
            msg="新增";
            order.setCreateTime(time).setCreatorId(userId).setCreatorEnterpriseId(orgId);
        } else {
            msg="编辑";
            order.setModifierId(userId).setModifyTime(time);
        }

        if (!insertOrUpdate(order)) {
            log.error("保存辅材消耗状态失败，调用{}的{}方法出错", "ErpAuxiliaryMaterialConsumptionServiceImpl", "saveAuxiliaryMaterialConsumption()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.ERP_AUXILIARY_MATERIAL_CONSUMPTION_SAVE_FAILED);

        }
        // 保存操作日志
        erpOperationLogService.saveLog(order.getCompanyId(),userInfo,order.getId(),"auxiliary_material_consumption",msg);

        return order.getId();
    }

    /**
     * 删除辅材消耗
     *
     * @param userInfo
     * @param id
     */

    @Transactional(rollbackFor = Exception.class)
    @Override
    public void deleteAuxiliaryMaterialConsumptionById(AuthPlatformUserInfo userInfo, Integer id) {

        Integer userId = userInfo.getId();
        Date time = new Date();
        ErpAuxiliaryMaterialConsumption exist = selectOne(new EntityWrapper<ErpAuxiliaryMaterialConsumption>().eq("id", id).eq("deleted", 0));
        if(ObjectUtils.isEmpty(exist)){
            log.info("根据id删除辅材消耗，没有找到相应数据，id为："+id);
            return;
        }
        if (!updateById(new ErpAuxiliaryMaterialConsumption().setId(id).setDeleted(1).setModifierId(userId).setModifyTime(time))) {
            log.error("删除辅材消耗状态失败，调用{}的{}方法出错", "ErpAuxiliaryMaterialConsumptionServiceImpl", "deleteAuxiliaryMaterialConsumptionById()");
            throw new BusinessException(ResCodeEnum.DELETE_FAILED, ExceptionMessageEnum.ERP_AUXILIARY_MATERIAL_CONSUMPTION_DELETE_FAILED);
        }


    }

    /**
     * 根据id查询辅材消耗详情
     *
     * @param id
     * @return
     */
    @Override
    public ErpAuxiliaryMaterialConsumptionDTO getAuxiliaryMaterialConsumptionById(Integer id) {
        ErpAuxiliaryMaterialConsumptionDTO dto = new ErpAuxiliaryMaterialConsumptionDTO();

        ErpAuxiliaryMaterialConsumption m = selectOne(new EntityWrapper<ErpAuxiliaryMaterialConsumption>().eq("id",id).eq("deleted",0));
        if (ObjectUtils.isEmpty(m)) {
            log.info("根据id查询辅材消耗详情,没有找到相关数据，id为："+id);
            return dto;
        }

        List<AuthEnterpriseFeignDetailDTO> companyList = authEnterpriseFeignClient.getEnterpriseMoreDetail(Lists.newArrayList(m.getCompanyId())).getObject();
        if(!CollectionUtils.isEmpty(companyList)){
            m.setCompanyName(companyList.get(0).getName());
        }
        m.setFurnaceNumber(commonMapper.getFurnaceNameById(m.getFurnaceId()));
        dto = BeanUtils.copyProperties(m, ErpAuxiliaryMaterialConsumptionDTO.class);

        return dto;
    }
}
