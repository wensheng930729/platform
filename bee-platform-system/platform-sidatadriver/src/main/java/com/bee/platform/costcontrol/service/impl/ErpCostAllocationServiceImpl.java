package com.bee.platform.costcontrol.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.mapper.Wrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.service.JedisService;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.costcontrol.dao.mapper.ErpCostAllocationMapper;
import com.bee.platform.costcontrol.entity.ErpCostAllocationCr;
import com.bee.platform.costcontrol.entity.ErpCostMaterialBatchSimulation;
import com.bee.platform.costcontrol.entity.ErpCostSimulationCr;
import com.bee.platform.costcontrol.service.ErpCostAllocationService;
import com.bee.platform.costcontrol.service.ErpCostMaterialBatchSimulationService;
import com.bee.platform.costcontrol.service.ErpCostSimulationService;
import com.bee.platform.costcontroller.dto.ErpCostAllocationBoxDTO;
import com.bee.platform.costcontroller.dto.ErpCostAllocationDetailDTO;
import com.bee.platform.costcontroller.dto.ErpCostAllocationQueryDTO;
import com.bee.platform.costcontroller.rq.ErpCostAllocationAddRQ;
import com.bee.platform.costcontroller.rq.ErpCostAllocationQueryRQ;
import com.bee.platform.costcontroller.rq.ErpCostAllocationSwitchRQ;
import com.bee.platform.costcontroller.rq.ErpCostAllocationUpdateRQ;
import com.bee.platform.datadriver.service.CommonService;
import com.bee.platform.user.authority.dto.AuthEnterpriseFlatDTO;
import com.bee.platform.user.dto.AuthUserBoxDTO;
import com.bee.platform.user.service.feign.AuthEnterpriseFeignClient;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * <p>
 * erp成本小工具-成本配置 服务实现类
 * </p>
 *
 * @author liliang123
 * @since 2019-06-24
 */
@Slf4j
@Service
public class ErpCostAllocationServiceImpl extends ServiceImpl<ErpCostAllocationMapper, ErpCostAllocationCr> implements ErpCostAllocationService {

    @Autowired
    private JedisService jedisService;
    @Autowired
    private ErpCostAllocationMapper costAllocationMapper;
    @Autowired
    private AuthEnterpriseFeignClient enterpriseFeignClient;
    @Autowired
    private ErpCostSimulationService costSimulationService;
    @Autowired
    private ErpCostMaterialBatchSimulationService costMaterialBatchSimulationService;
    @Autowired
    private CommonService commonService;

    /**
     * 添加erp成本配置
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult addCostAllocation(ErpCostAllocationAddRQ rq, AuthPlatformUserInfo userInfo) {
        // 校验参数
        int siC = rq.getSiliconContent().add(rq.getCarbonContent()).compareTo(BigDecimal.valueOf(100));
        if (siC >= 0) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SI_C_CONTENT_TOO_BIG);
        }
        // 兰炭和焦炭比例和必须为100%
        if (rq.getCokeRatio().add(rq.getSemiCokeRatio()).compareTo(BigDecimal.valueOf(100)) != 0) {
            return ResponseResult.buildResponseResult(ResCodeEnum.COKERATIO_SEMICOKERATIO_ADD_IS_ONE);
        }
        ErpCostAllocationCr costAllocation = BeanUtils.copyProperties(rq, ErpCostAllocationCr.class);
        costAllocation.setCreatorId(userInfo.getId())
                .setCreator(userInfo.getNickname())
                .setCreateTime(new Date())
                .setStatus(Status.TRUE.getKey())
                .setDeleted(Status.FALSE.getKey());
        // 如果自定义名称 则判断是否重复
        if (!StringUtils.isBlank(rq.getName())) {
            int nameCheck = this.selectCount(new EntityWrapper<>(new ErpCostAllocationCr()
                    .setCompanyId(rq.getCompanyId())
                    .setName(rq.getName())
                    .setDeleted(Status.FALSE.getKey())));
            if (nameCheck > 0) {
                return ResponseResult.buildResponseResult(ResCodeEnum.NAME_EXIST);
            }
        } else {
            // 手动生成编码
            String redisKey = "siCostAllocation" + rq.getCompanyId();
            int valueCode = jedisService.incrOne(redisKey);
            costAllocation.setName(LocalDate.now().toString() + "-"
                    + commonService.getCode(valueCode + "", 3));
        }
        if (!this.insert(costAllocation)) {
            log.error("添加成本配置失败 类：{} 方法：{}", "ErpCostAllocationServiceImpl", "addCostAllocation");
            return ResponseResult.buildResponseResult(ResCodeEnum.ADD_FAILED);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * 删除配置
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult deleteCostAllocation(Integer id) {
        ErpCostAllocationCr costAllocation = this.selectById(id);
        if (ObjectUtils.isEmpty(costAllocation)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ERP_COST_ALLOCATION_NOT_FOUND);
        }
        //  判断改配置是否使用
        int costSimulationCount = costSimulationService.selectCount(new EntityWrapper<>(new ErpCostSimulationCr()
                .setCostAllocationId(id)
                .setDeleted(Status.FALSE.getKey())));
        int costMaterialBatchSimulationCount = costMaterialBatchSimulationService.selectCount(new EntityWrapper<>(new ErpCostMaterialBatchSimulation()
                .setDeleted(Status.FALSE.getKey())
                .setAllocationId(id)));
        if ((costSimulationCount + costMaterialBatchSimulationCount) > 0) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ERP_COST_ALLOCATION_DENY_DELETE);
        }

        costAllocation.setDeleted(Status.TRUE.getKey()).setDeletedTime(new Date());
        if (!this.updateById(costAllocation)) {
            log.error("删除成本配置失败 类：{} 方法：{}", "ErpCostAllocationServiceImpl", "deleteCostAllocation");
            return ResponseResult.buildResponseResult(ResCodeEnum.DELETE_FAIL);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * 更新成本配置
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult updateCostAllocation(ErpCostAllocationUpdateRQ rq) {
        // 校验参数
        int siC = rq.getSiliconContent().add(rq.getCarbonContent()).compareTo(BigDecimal.valueOf(100));
        if (siC >= 0) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SI_C_CONTENT_TOO_BIG);
        }
        if (rq.getCokeRatio().add(rq.getSemiCokeRatio()).compareTo(BigDecimal.valueOf(1)) != 0) {
            return ResponseResult.buildResponseResult(ResCodeEnum.COKERATIO_SEMICOKERATIO_ADD_IS_ONE);
        }

        Integer id = rq.getId();
        ErpCostAllocationCr costAllocation = this.selectById(id);
        if (ObjectUtils.isEmpty(costAllocation)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ERP_COST_ALLOCATION_NOT_FOUND);
        }
        //  判断改配置是否使用
        int costSimulationCount = costSimulationService.selectCount(new EntityWrapper<>(new ErpCostSimulationCr()
                .setCostAllocationId(id)
                .setDeleted(Status.FALSE.getKey())));
        int costMaterialBatchSimulationCount = costMaterialBatchSimulationService.selectCount(new EntityWrapper<>(new ErpCostMaterialBatchSimulation()
                .setDeleted(Status.FALSE.getKey())
                .setAllocationId(id)));
        if ((costSimulationCount + costMaterialBatchSimulationCount) > 0) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ERP_COST_ALLOCATION_DENY_UPDATE);
        }

        org.springframework.beans.BeanUtils.copyProperties(rq, costAllocation);
        costAllocation.setUpdateTime(new Date());
        if (!this.updateById(costAllocation)) {
            log.error("更新成本配置失败 类：{} 方法：{}", "ErpCostAllocationServiceImpl", "updateCostAllocation");
            return ResponseResult.buildResponseResult(ResCodeEnum.UPDATE_FAIL);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * 切换配置的启用和禁用
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult updateStatus(ErpCostAllocationSwitchRQ rq) {
        Integer id = rq.getId();
        ErpCostAllocationCr costAllocation = this.selectById(id);
        if (ObjectUtils.isEmpty(costAllocation)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ERP_COST_ALLOCATION_NOT_FOUND);
        }
        // 如果是禁用--判断改配置是否使用
        if (rq.getStatus().equals(Status.FALSE.getKey())) {
            int costSimulationCount = costSimulationService.selectCount(new EntityWrapper<>(new ErpCostSimulationCr()
                    .setCostAllocationId(id)
                    .setDeleted(Status.FALSE.getKey())));
            int costMaterialBatchSimulationCount = costMaterialBatchSimulationService.selectCount(new EntityWrapper<>(new ErpCostMaterialBatchSimulation()
                    .setDeleted(Status.FALSE.getKey())
                    .setAllocationId(id)));
            if (costSimulationCount > 0 || costMaterialBatchSimulationCount > 0) {
                return ResponseResult.buildResponseResult(ResCodeEnum.ERP_COST_ALLOCATION_DENY_FORBIDDEN);
            }
        }
        costAllocation.setStatus(rq.getStatus());
        if (!this.updateById(costAllocation)) {
            log.error("更新成本配置状态失败 类：{} 方法：{}", "ErpCostAllocationServiceImpl", "updateStatus");
            return ResponseResult.buildResponseResult(ResCodeEnum.UPDATE_FAIL);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * 查询配置列表
     */
    @Override
    public ResponseResult<List<ErpCostAllocationQueryDTO>> getCostList(Integer companyId, ErpCostAllocationQueryRQ rq, AuthPlatformUserInfo userInfo, Page page) {
        Pagination pagination = PageUtils.transFromPage(page);
        // 本身及其子公司
        List<AuthEnterpriseFlatDTO> enterpriseFlatDTOS = enterpriseFeignClient.getEnterpriseFlatByUser(userInfo.getSysToken()).getObject();
        // 构建条件
        Wrapper<ErpCostAllocationCr> wrapper = new EntityWrapper<>();
        wrapper.eq("deleted", Status.FALSE.getKey());
        if (!ObjectUtils.isEmpty(rq.getCompanyId())) {
            wrapper.eq("company_id", rq.getCompanyId());
        } else if (!ObjectUtils.isEmpty(companyId)) {
            wrapper.eq("company_id", companyId);
        } else {
            List<Integer> ids = null;
            if (!CollectionUtils.isEmpty(enterpriseFlatDTOS)) {
                ids = enterpriseFlatDTOS.stream().map(a -> a.getValue()).collect(Collectors.toList());
            } else {
                ids = Lists.newArrayList(1);
                ids.add(userInfo.getOrgId());
            }
            wrapper.in("company_id", ids);
        }
        if (!StringUtils.isBlank(rq.getName())) {
            wrapper.eq("name", rq.getName());
        }
        if (!ObjectUtils.isEmpty(rq.getStatus())) {
            wrapper.eq("status", rq.getStatus());
        }
        List<ErpCostAllocationCr> costAllocationList = costAllocationMapper.selectPage(pagination, wrapper);
        List<ErpCostAllocationQueryDTO> dtoList = BeanUtils.assemble(ErpCostAllocationQueryDTO.class, costAllocationList);
        // 公司名称
        Map<Integer, String> companyMap = Maps.newHashMap();
        ;
        if (!CollectionUtils.isEmpty(enterpriseFlatDTOS)) {
            companyMap = enterpriseFlatDTOS.stream().collect(Collectors.toMap(a -> a.getValue(), b -> b.getLabel()));
        }
        // 创建人姓名查询
        Map<Integer, String> nameMap = Maps.newHashMap();
        ;
        if (!CollectionUtils.isEmpty(dtoList)) {
            List<Integer> creatorIds = dtoList.stream().map(a -> a.getCreatorId()).collect(Collectors.toList());
            if (!CollectionUtils.isEmpty(creatorIds)) {
                List<AuthUserBoxDTO> creatorList = costAllocationMapper.queryCreator(creatorIds);
                if (!CollectionUtils.isEmpty(creatorList)) {
                    nameMap = creatorList.stream().collect(Collectors.toMap(a -> a.getId(), b -> b.getName()));
                }
            }
        }
        // 设置公司名称
        for (ErpCostAllocationQueryDTO dto : dtoList) {
            dto.setCompanyName(companyMap.get(dto.getCompanyId()))
                    .setCreator(nameMap.get(dto.getCreatorId()));
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dtoList, PageUtils.transToPage(pagination));
    }

    /**
     * 查询配置详情
     */
    @Override
    public ResponseResult<ErpCostAllocationDetailDTO> getCostAllocationDetail(Integer id) {
        ErpCostAllocationCr costAllocation = this.selectById(id);
        if (ObjectUtils.isEmpty(costAllocation)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ERP_COST_ALLOCATION_NOT_FOUND);
        }
        ErpCostAllocationDetailDTO detailDTO = BeanUtils.copyProperties(costAllocation, ErpCostAllocationDetailDTO.class);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, detailDTO);
    }

    /**
     * 查询配置的下拉框列表
     */
    @Override
    public ResponseResult<List<ErpCostAllocationBoxDTO>> getCostAllocationBox(AuthPlatformUserInfo userInfo) {
        List<AuthEnterpriseFlatDTO> enterpriseFlatDTOS = enterpriseFeignClient.getEnterpriseFlatByUser(userInfo.getSysToken()).getObject();
        List<Integer> ids = Lists.newArrayList();
        if (!CollectionUtils.isEmpty(enterpriseFlatDTOS)) {
            ids = enterpriseFlatDTOS.stream().map(a -> a.getValue()).collect(Collectors.toList());
        } else {
            ids.add(userInfo.getOrgId());
        }
        List<ErpCostAllocationCr> costAllocationList = this.selectList(new EntityWrapper<>(new ErpCostAllocationCr()
                .setDeleted(Status.FALSE.getKey())
                .setStatus(Status.TRUE.getKey()))
                .in("company_id", ids));
        List<ErpCostAllocationBoxDTO> dtoList = BeanUtils.assemble(ErpCostAllocationBoxDTO.class, costAllocationList);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dtoList);
    }

    /**
     * 查询配置的下拉框列表
     */
    @Override
    public ResponseResult<List<ErpCostAllocationBoxDTO>> getCostAllocationBoxByCompanyId(Integer companyId) {
        List<ErpCostAllocationCr> costAllocationList = this.selectList(new EntityWrapper<>(new ErpCostAllocationCr()
                .setDeleted(Status.FALSE.getKey())
                .setStatus(Status.TRUE.getKey())
                .setCompanyId(companyId)));
        if (CollectionUtils.isEmpty(costAllocationList)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, new ArrayList<>(0));
        }
        List<ErpCostAllocationBoxDTO> dtoList = BeanUtils.assemble(ErpCostAllocationBoxDTO.class, costAllocationList);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dtoList);
    }
}
