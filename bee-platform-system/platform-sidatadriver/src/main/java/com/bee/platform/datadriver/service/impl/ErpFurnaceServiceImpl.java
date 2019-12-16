package com.bee.platform.datadriver.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.constants.enums.EnumCommon;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.datadriver.dao.mapper.*;
import com.bee.platform.datadriver.dto.ErpFurnaceBoxDTO;
import com.bee.platform.datadriver.dto.ErpFurnaceListDTO;
import com.bee.platform.datadriver.dto.ErpFurnaceOneDTO;
import com.bee.platform.datadriver.entity.*;
import com.bee.platform.datadriver.rq.ErpFurnacaAddRQ;
import com.bee.platform.datadriver.rq.ErpFurnacaUpdateRQ;
import com.bee.platform.datadriver.service.ErpFurnaceService;
import com.bee.platform.user.authority.dto.AuthEnterpriseFlatDTO;
import com.bee.platform.user.service.feign.AuthEnterpriseFeignClient;
import com.bee.platform.user.service.feign.UserInfoFeignClient;
import com.google.common.collect.Maps;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * <p>
 * 炉子档案 服务实现类
 * </p>
 *
 * @author hongchuan.he
 * @since 2019-05-27
 */
@Slf4j
@Service
public class ErpFurnaceServiceImpl extends ServiceImpl<ErpFurnaceMapper, ErpFurnace> implements ErpFurnaceService {

    @Autowired
    private UserInfoFeignClient userInfoFeignClient;
    @Autowired
    private ErpFurnaceMapper furnaceMapper;
    @Autowired
    private AuthEnterpriseFeignClient authEnterpriseFeignClient;
    @Autowired
    private ErpAuxiliaryMaterialConsumptionMapper materialConsumptionMapper;
    @Autowired
    private ErpOutOfStockOrderMapper outOfStockOrderMapper;
    @Autowired
    private ErpRepositoryReceiptMapper repositoryReceiptMapper;
    @Autowired
    private ErpTestReportMapper testReportMapper;
    @Autowired
    private ErpWarehousingOrderMapper warehousingOrderMapper;
    private static Integer ZERO = 0;


    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<Integer> addFurnace(ErpFurnacaAddRQ rq, AuthPlatformUserInfo userInfo) {
        // 校验设备名称是否重复
        List<ErpFurnace> furnaceNameCheckList = this.selectList(new EntityWrapper<>(new ErpFurnace()
                .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())
                .setOrgId(rq.getOrgId())
                .setName(rq.getName())));
        if (!CollectionUtils.isEmpty(furnaceNameCheckList)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FURNACE_NAME_EXIST);
        }
        ErpFurnace furnace = BeanUtils.copyProperties(rq, ErpFurnace.class);
        furnace.setCreateUser(userInfo.getId())
                .setOperateId(userInfo.getId())
                .setCreateTime(new Date())
                .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey());
        if (!this.insert(furnace)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ADD_FAILED);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, furnace.getId());
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<Integer> deleteFurnace(Integer id, AuthPlatformUserInfo userInfo) {
        // 校验设备是否被使用
        if (this.checkFurnaceUseInfo(id)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FURNACE_IS_USED);
        }
        ErpFurnace furnace = furnaceMapper.selectOne(new ErpFurnace().setId(id).setDeleted(EnumCommon.IsDeleted.not_Delete.getKey()));
        if (ObjectUtils.isEmpty(furnace)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FURNACE_NOT_EXIST);
        }
        furnace.setDeleted(EnumCommon.IsDeleted.is_Delete.getKey())
                .setDeletedTime(new Date());
        if (!this.updateById(furnace)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.DELETE_FAIL);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, furnace.getId());
    }

    /**
     * 校验设备是否被使用
     *
     * @param id
     * @return
     */
    private boolean checkFurnaceUseInfo(Integer id) {
        // 辅材消耗
        Integer materialConsumption = materialConsumptionMapper.selectCount(new EntityWrapper<>(new ErpAuxiliaryMaterialConsumption()
                .setDeleted(Status.FALSE.getKey()).setFurnaceId(id)));
        // 领料出库
        Integer outOfStock = outOfStockOrderMapper.selectCount(new EntityWrapper<>(new ErpOutOfStockOrder()
                .setDeleted(Status.FALSE.getKey()).setFurnaceNumberId(id)));
        // 原料入库,成品出库
        Integer repositoryReceipt = repositoryReceiptMapper.selectCount(new EntityWrapper<>(new ErpRepositoryReceipt()
                .setDeleted(Status.FALSE.getKey()).setFurnaceId(id)));
        // 化验单
        Integer testReport = testReportMapper.selectCount(new EntityWrapper<>(new ErpTestReport()
                .setDeleted(Status.FALSE.getKey()).setBoilerId(id.toString())));
        // 成品入库
        Integer warehousingOrder = warehousingOrderMapper.selectCount(new EntityWrapper<>(new ErpWarehousingOrder()
                .setDeleted(Status.FALSE.getKey()).setFurnaceId(id)));

        Integer total = materialConsumption + outOfStock + repositoryReceipt + testReport + warehousingOrder;

        return total > ZERO;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<Integer> updateFurnace(ErpFurnacaUpdateRQ rq, AuthPlatformUserInfo userInfo) {
        ErpFurnace furnace = furnaceMapper.selectOne(new ErpFurnace().setId(rq.getId()).setDeleted(EnumCommon.IsDeleted.not_Delete.getKey()));
        if (ObjectUtils.isEmpty(furnace)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FURNACE_NOT_EXIST);
        }
        // 校验设备名称是否重复
        if (!rq.getName().equals(furnace.getName())) {
            List<ErpFurnace> furnaceNameCheckList = this.selectList(new EntityWrapper<>(new ErpFurnace()
                    .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())
                    .setOrgId(rq.getOrgId())
                    .setName(rq.getName())));
            if (!CollectionUtils.isEmpty(furnaceNameCheckList)) {
                return ResponseResult.buildResponseResult(ResCodeEnum.FURNACE_NAME_EXIST);
            }
        }
        org.springframework.beans.BeanUtils.copyProperties(rq, furnace);
        furnace.setOperateId(userInfo.getId());
        furnace.setUpdateTime(new Date());
        if (!this.updateById(furnace)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.UPDATE_FAIL);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, furnace.getId());
    }

    @Override
    public ResponseResult<Integer> updateFurnaceStatus(Integer id, Integer status, AuthPlatformUserInfo userInfo) {
        ErpFurnace furnace = this.selectOne(new EntityWrapper<>(new ErpFurnace().setId(id).setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())));
        if (ObjectUtils.isEmpty(furnace)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FURNACE_NOT_EXIST);
        }
        furnace.setStatus(status).setOperateId(userInfo.getId()).setUpdateTime(new Date());
        if (furnaceMapper.updateById(furnace) != 1) {
            return ResponseResult.buildResponseResult(ResCodeEnum.UPDATE_FAIL);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, furnace.getId());
    }

    /**
     * 根据id查询炉子信息
     */
    @Override
    public ResponseResult<ErpFurnaceOneDTO> getById(Integer id) {
        ErpFurnace furnace = this.selectOne(new EntityWrapper<>(new ErpFurnace()
                .setId(id)
                .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())));
        if (ObjectUtils.isEmpty(furnace)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FURNACE_NOT_EXIST);
        }
        ErpFurnaceOneDTO dto = BeanUtils.copyProperties(furnace, ErpFurnaceOneDTO.class);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
    }

    @Override
    public ResponseResult<List<ErpFurnaceListDTO>> query(Integer companyId, Integer status, Pagination pagination) {
        // 公司名称
        List<AuthEnterpriseFlatDTO> enterpriseFlatDTOS = authEnterpriseFeignClient.getEnterpriseFlatByCompanyId(companyId).getObject();
        Map<Integer, String> enterpriseMap = Maps.newHashMap();
        if (!org.springframework.util.CollectionUtils.isEmpty(enterpriseFlatDTOS)) {
            enterpriseMap = enterpriseFlatDTOS.stream().collect(Collectors.toMap(a -> a.getValue(), b -> b.getLabel()));
        }
        // 查询设备
        ErpFurnace furnace = new ErpFurnace()
                .setOrgId(companyId)
                .setDeleted(Status.FALSE.getKey());
        if (!ObjectUtils.isEmpty(status)) {
            furnace.setStatus(status);
        }
        List<ErpFurnace> erpFurnaceList = furnaceMapper.selectPage(pagination, new EntityWrapper<>(furnace));
        List<ErpFurnaceListDTO> furnaceListDTOS = BeanUtils.assemble(ErpFurnaceListDTO.class, erpFurnaceList);
        for (ErpFurnaceListDTO dto : furnaceListDTOS) {
            dto.setOrgName(enterpriseMap.get(dto.getOrgId()));
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, furnaceListDTOS, PageUtils.transToPage(pagination));
    }

    @Override
    public ResponseResult<List<ErpFurnaceBoxDTO>> queryFurnaceNum(Integer orgId) {
        List<ErpFurnace> furnaceList = furnaceMapper.selectList(new EntityWrapper<>(new ErpFurnace()
                .setOrgId(orgId)
                .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())
                .setStatus(EnumCommon.IsActive.is_active.getKey())));
        List<ErpFurnaceBoxDTO> furnaceBoxDTOS = BeanUtils.assemble(ErpFurnaceBoxDTO.class, furnaceList);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, furnaceBoxDTOS);
    }


    @Override
    public List<ErpFurnaceBoxDTO> getFurnaceList(AuthPlatformUserInfo userInfo, String sysToken) {
        Integer orgId = userInfo.getOrgId();
//        List<AuthEnterpriseFlatDTO> enterpriseFlatDTOS = authEnterpriseFeignClient.getEnterpriseFlatByUser(sysToken).getObject();
//        if (CollectionUtils.isEmpty(enterpriseFlatDTOS)) {
//            log.error("未查询到当前用户企业信息，无法查询炉子信息！ 类：{} 方法：{}", "ErpFurnaceServiceImpl", "getFurnaceList");
//            return Lists.newArrayList();
//        }
//        List<Integer> ids = enterpriseFlatDTOS.stream().map(a -> a.getValue()).collect(Collectors.toList());
//
//        if (org.springframework.util.CollectionUtils.isEmpty(ids)) {
//            return Lists.newArrayList();
//        }
        List<ErpFurnace> erpRepositories = selectList(new EntityWrapper<ErpFurnace>()
                .eq("status", Status.TRUE.getKey())
                .eq("deleted", Status.FALSE.getKey())
                .eq("org_id", orgId));
        return BeanUtils.assemble(ErpFurnaceBoxDTO.class, erpRepositories);
    }
}
