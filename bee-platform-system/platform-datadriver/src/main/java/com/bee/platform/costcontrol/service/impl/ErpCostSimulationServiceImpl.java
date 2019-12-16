package com.bee.platform.costcontrol.service.impl;

import com.alibaba.fastjson.JSON;
import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.mapper.Wrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.costcontrol.dao.mapper.ErpCostSimulationMapper;
import com.bee.platform.costcontrol.entity.ErpCostAllocationCr;
import com.bee.platform.costcontrol.entity.ErpCostIndexInputCr;
import com.bee.platform.costcontrol.entity.ErpCostSimulationCr;
import com.bee.platform.costcontrol.entity.ErpCostSimulationComputedResultCr;
import com.bee.platform.costcontrol.service.ErpCostAllocationService;
import com.bee.platform.costcontrol.service.ErpCostIndexInputService;
import com.bee.platform.costcontrol.service.ErpCostSimulationComputedResultService;
import com.bee.platform.costcontrol.service.ErpCostSimulationService;
import com.bee.platform.costcontroller.dto.ErpCostSimulationComputedResultDTO;
import com.bee.platform.costcontroller.dto.ErpCostSimulationDetailDTO;
import com.bee.platform.costcontroller.dto.ErpCostSimulationQueryDTO;
import com.bee.platform.costcontroller.enums.EnumCostSimulationComputedType;
import com.bee.platform.costcontroller.rq.*;
import com.bee.platform.user.authority.dto.AuthEnterpriseFlatDTO;
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
import java.math.RoundingMode;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * <p>
 * 成本模拟基础配置 服务实现类
 * </p>
 *
 * @author liliang123
 * @since 2019-06-25
 */
@Slf4j
@Service
public class ErpCostSimulationServiceImpl extends ServiceImpl<ErpCostSimulationMapper, ErpCostSimulationCr> implements ErpCostSimulationService {

    @Autowired
    private ErpCostIndexInputService costIndexInputService;
    @Autowired
    private ErpCostSimulationComputedResultService costSimulationComputedResultService;
    @Autowired
    private AuthEnterpriseFeignClient enterpriseFeignClient;
    @Autowired
    private ErpCostSimulationMapper costSimulationMapper;
    @Autowired
    private ErpCostAllocationService costAllocationService;

    /**
     * 添加成本模拟
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult addCostSimulation(ErpCostSimulationAddRQ rq, AuthPlatformUserInfo userInfo) {
        ErpCostSimulationCr costSimulation = BeanUtils.copyProperties(rq, ErpCostSimulationCr.class);
        costSimulation.setInquirerId(userInfo.getId())
                .setCostAllocation(JSON.toJSONString(rq.getCostAllocation()))
                .setStatus(Status.TRUE.getKey())
                .setDeleted(Status.FALSE.getKey())
                .setCreateTime(new Date())
                .setCreator(userInfo.getNickname())
                .setCreatorId(userInfo.getId());
        // 插入模拟基础数据
        this.insert(costSimulation);
        // 插入指数录入
        List<ErpCostIndexInputRQ> costIndexInputRQList = rq.getCostIndexInputList();
        List<ErpCostIndexInputCr> costIndexInputList = BeanUtils.assemble(ErpCostIndexInputCr.class, costIndexInputRQList);
        costIndexInputList.forEach(a -> a.setCostSimulationId(costSimulation.getId())
                .setStatus(Status.TRUE.getKey())
                .setDeleted(Status.FALSE.getKey())
                .setCreateTime(new Date())
                .setCreator(userInfo.getNickname())
                .setCreatorId(userInfo.getId()));
        costIndexInputService.insertBatch(costIndexInputList);
        // 插入计算结果
        List<ErpCostSimulationComputedResultRQ> computedResultRQList = rq.getCostSimulationComputedResultList();
        List<ErpCostSimulationComputedResultCr> computedResultList = BeanUtils.assemble(ErpCostSimulationComputedResultCr.class, computedResultRQList);
        computedResultList.forEach(a -> a.setCostSimulationId(costSimulation.getId())
                .setStatus(Status.TRUE.getKey())
                .setDeleted(Status.FALSE.getKey())
                .setCreateTime(new Date())
                .setCreator(userInfo.getNickname())
                .setCreatorId(userInfo.getId()));
        costSimulationComputedResultService.insertBatch(computedResultList);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * 删除成本模拟
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult deleteCostSimulation(Integer id) {
        ErpCostSimulationCr costSimulation = this.selectById(id);
        if (ObjectUtils.isEmpty(costSimulation)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ERP_COST_SIMULATION_NOT_FOUND);
        }
        costSimulation.setDeleted(Status.TRUE.getKey())
                .setDeletedTime(new Date());
        // 更新基础配置
        this.updateById(costSimulation);
        // 更新指标录入
        costIndexInputService.update(new ErpCostIndexInputCr()
                        .setDeleted(Status.TRUE.getKey())
                        .setDeletedTime(new Date()),
                new EntityWrapper<ErpCostIndexInputCr>()
                        .eq("cost_simulation_id", id));
        // 更新计算结果
        costSimulationComputedResultService.update(new ErpCostSimulationComputedResultCr()
                        .setDeleted(Status.TRUE.getKey())
                        .setDeletedTime(new Date()),
                new EntityWrapper<ErpCostSimulationComputedResultCr>()
                        .eq("cost_simulation_id", id));
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * 修改成本模拟
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult updateCostSimulation(ErpCostSimulationUpdateRQ rq, AuthPlatformUserInfo userInfo) {
        Integer id = rq.getId();
        ErpCostSimulationCr costSimulation = this.selectOne(new EntityWrapper<>(new ErpCostSimulationCr()
                .setId(id)
                .setStatus(Status.TRUE.getKey())
                .setDeleted(Status.FALSE.getKey())));
        if (ObjectUtils.isEmpty(costSimulation)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ERP_COST_SIMULATION_NOT_FOUND);
        }
        org.springframework.beans.BeanUtils.copyProperties(rq, costSimulation);
        costSimulation.setCostAllocation(JSON.toJSONString(rq.getCostAllocation()))
                .setUpdateTime(new Date());
        this.updateById(costSimulation);
        // 更新指标录入
        costIndexInputService.update(new ErpCostIndexInputCr().setDeleted(Status.TRUE.getKey()).setDeletedTime(new Date()),
                new EntityWrapper<>(new ErpCostIndexInputCr().setCostSimulationId(id)));
        List<ErpCostIndexInputRQ> costIndexInputRQList = rq.getCostIndexInputList();
        List<ErpCostIndexInputCr> costIndexInputList = BeanUtils.assemble(ErpCostIndexInputCr.class, costIndexInputRQList);
        costIndexInputList.forEach(a -> a.setCostSimulationId(id)
                .setStatus(Status.TRUE.getKey())
                .setDeleted(Status.FALSE.getKey())
                .setCreateTime(new Date())
                .setCreator(userInfo.getNickname())
                .setCreatorId(userInfo.getId()));
        costIndexInputService.insertBatch(costIndexInputList);
        // 更新计算结果
        costSimulationComputedResultService.update(new ErpCostSimulationComputedResultCr().setDeleted(Status.TRUE.getKey()).setDeletedTime(new Date()),
                new EntityWrapper<>(new ErpCostSimulationComputedResultCr().setCostSimulationId(id)));
        List<ErpCostSimulationComputedResultRQ> computedResultRQList = rq.getCostSimulationComputedResultList();
        List<ErpCostSimulationComputedResultCr> computedResultList = BeanUtils.assemble(ErpCostSimulationComputedResultCr.class, computedResultRQList);
        computedResultList.forEach(a -> a.setCostSimulationId(costSimulation.getId())
                .setStatus(Status.TRUE.getKey())
                .setDeleted(Status.FALSE.getKey())
                .setCreateTime(new Date())
                .setCreator(userInfo.getNickname())
                .setCreatorId(userInfo.getId()));
        costSimulationComputedResultService.insertBatch(computedResultList);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * 查询成本模拟列表
     */
    @Override
    public ResponseResult<List<ErpCostSimulationQueryDTO>> getSimulationList(Integer companyId, ErpCostSimulationQueryRQ rq, AuthPlatformUserInfo userInfo, Page page) {
        Pagination pagination = PageUtils.transFromPage(page);
        List<AuthEnterpriseFlatDTO> enterpriseFlatDTOS = enterpriseFeignClient.getEnterpriseFlatByUser(userInfo.getSysToken()).getObject();
        // 条件
        Wrapper<ErpCostSimulationCr> wrapper = new EntityWrapper<>();
        List<Integer> ids = null;
        if (!ObjectUtils.isEmpty(companyId)) {
            wrapper.eq("company_id", companyId);
        } else {
            if (!CollectionUtils.isEmpty(enterpriseFlatDTOS)) {
                ids = enterpriseFlatDTOS.stream().map(a -> a.getValue()).collect(Collectors.toList());
            } else {
                ids = Lists.newArrayList(1);
                ids.add(userInfo.getOrgId());
            }
            wrapper.in("company_id", ids);
        }
        wrapper.eq("deleted", Status.FALSE.getKey())
                .eq("status", Status.TRUE.getKey());
        if (!StringUtils.isBlank(rq.getRawMaterial())) {
            wrapper.like("raw_material", rq.getRawMaterial());
        }
        if (!StringUtils.isBlank(rq.getSupplier())) {
            wrapper.like("supplier", rq.getSupplier());
        }
        if (!ObjectUtils.isEmpty(rq.getStatus())) {
            wrapper.eq("status", rq.getStatus());
        }
        if (!ObjectUtils.isEmpty(rq.getCostAllocationId())) {
            wrapper.eq("cost_allocation_id", rq.getCostAllocationId());
        }
        List<ErpCostSimulationCr> costSimulationList = costSimulationMapper.selectPage(pagination, wrapper);
        if (CollectionUtils.isEmpty(costSimulationList)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, new ArrayList<>(0), PageUtils.transToPage(pagination));
        }
        List<Integer> simulationIds = costSimulationList.stream().map(a -> a.getId()).collect(Collectors.toList());
        // 查询计算结果
        List<ErpCostSimulationComputedResultCr> computedResultList = costSimulationComputedResultService.selectList(new EntityWrapper<>(new ErpCostSimulationComputedResultCr()
                .setDeleted(Status.FALSE.getKey())
                .setStatus(Status.TRUE.getKey()))
                .in("cost_simulation_id", simulationIds));
        Map<String, BigDecimal> computedMap = Maps.newHashMap();
        if (!CollectionUtils.isEmpty(computedResultList)) {
            for (ErpCostSimulationComputedResultCr computedResult : computedResultList) {
                String key = computedResult.getCostSimulationId().toString() + computedResult.getType();
                computedMap.put(key, computedResult.getFiftyFullCost());
            }
        }
        // 查询成本配置
        List<ErpCostAllocationCr> costAllocationList = costAllocationService.selectList(new EntityWrapper<>(new ErpCostAllocationCr()
                .setDeleted(Status.FALSE.getKey())
                .setStatus(Status.TRUE.getKey())).in("company_id", ids));
        Map<Integer, String> allocationMap = Maps.newHashMap();
        if (!CollectionUtils.isEmpty(costAllocationList)) {
            allocationMap = costAllocationList.stream().collect(Collectors.toMap(a -> a.getId(), b -> b.getName()));
        }
        // 拼装结果
        List<ErpCostSimulationQueryDTO> resultList = Lists.newArrayList();
        for (ErpCostSimulationCr costSimulation : costSimulationList) {
            ErpCostSimulationQueryDTO dto = BeanUtils.copyProperties(costSimulation, ErpCostSimulationQueryDTO.class);
            String idStr = dto.getId().toString();
            dto.setCostAllocationName(allocationMap.get(costSimulation.getCostAllocationId()))
                    .setSelfCheckCost(computedMap.get(idStr + EnumCostSimulationComputedType.SELF.getKey()))
                    .setFastCheckCost(computedMap.get(idStr + EnumCostSimulationComputedType.FAST.getKey()))
                    .setCommodityCheckCost(computedMap.get(idStr + EnumCostSimulationComputedType.COMMODITY.getKey()));
            resultList.add(dto);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, resultList, PageUtils.transToPage(pagination));
    }

    /**
     * 查询成本模拟单个详情
     */
    @Override
    public ResponseResult<ErpCostSimulationDetailDTO> getCostSimulationDetail(Integer id) {
        ErpCostSimulationCr costSimulation = this.selectOne(new EntityWrapper<>(new ErpCostSimulationCr().setId(id).setDeleted(Status.FALSE.getKey())));
        if (ObjectUtils.isEmpty(costSimulation)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ERP_COST_SIMULATION_NOT_FOUND);
        }
        ErpCostSimulationDetailDTO detailDTO = BeanUtils.copyProperties(costSimulation, ErpCostSimulationDetailDTO.class);
        // 查询指标录入
        List<ErpCostIndexInputCr> costIndexInputList = costIndexInputService.selectList(new EntityWrapper<>(new ErpCostIndexInputCr()
                .setCostSimulationId(costSimulation.getId())
                .setDeleted(Status.FALSE.getKey())
                .setStatus(Status.TRUE.getKey())));
        List<ErpCostIndexInputRQ> costIndexInputRQList = BeanUtils.assemble(ErpCostIndexInputRQ.class, costIndexInputList);
        // 计算结果查询
        List<ErpCostSimulationComputedResultCr> computedResultList = costSimulationComputedResultService.selectList(new EntityWrapper<>(new ErpCostSimulationComputedResultCr()
                .setCostSimulationId(costSimulation.getId())
                .setDeleted(Status.FALSE.getKey())
                .setStatus(Status.TRUE.getKey())));
        List<ErpCostSimulationComputedResultRQ> computedResultRQList = BeanUtils.assemble(ErpCostSimulationComputedResultRQ.class, computedResultList);

        detailDTO.setCostAllocation(JSON.parseObject(costSimulation.getCostAllocation(), ErpCostAllocationSimulationRQ.class))
                .setCostIndexInputList(costIndexInputRQList)
                .setCostSimulationComputedResultList(computedResultRQList);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, detailDTO);
    }

    /**
     * 根据参数得出计算结果
     * 公式有问题！！！
     */
    @Override
    public ResponseResult<ErpCostSimulationComputedResultDTO> getCalculationResults(ErpCostSimulationCalculationRQ rq) {
        // 料批
        BigDecimal materialBatch = BigDecimal.valueOf(1);
        // 原料进口税
        BigDecimal rawMaterialImportTax = BigDecimal.valueOf(0.13);
        // 运费税
        BigDecimal freightTax = BigDecimal.valueOf(0.09);

        // 传入的成本模拟参数
        ErpCostAllocationSimulationRQ costAllocation = rq.getCostAllocation();
        // 传入的指数录入参数
        ErpCostIndexInputRQ costIndexInput = rq.getCostIndexInput();
        // 返回的结果
        ErpCostSimulationComputedResultDTO resultDTO = new ErpCostSimulationComputedResultDTO();

        /**-------------------成品品味----------------------*/
        // （Cr2O3*0.684*Cr回收率）/[（Cr2O3*0.684*Cr回收率+FeO*0.7778*Fe回收率）/（1-C含量-Si含量）]*100
        // Cr2O3
        BigDecimal chromiumTrioxide = costIndexInput.getChromiumTrioxide();
        // Cr回收率
        BigDecimal chromiumRecoveryRate = costAllocation.getChromiumRecoveryRate();
        // FeO
        BigDecimal ferrousOxide = costIndexInput.getFerrousOxide();
        // Fe回收率
        BigDecimal ironRecoveryRate = costAllocation.getIronRecoveryRate();
        // Si含量
        BigDecimal siliconContent = costAllocation.getSiliconContent();
        // C含量
        BigDecimal carbonContent = costAllocation.getCarbonContent();

        BigDecimal cr = chromiumTrioxide.multiply(BigDecimal.valueOf(0.684)).multiply(chromiumRecoveryRate);
        BigDecimal fe = ferrousOxide.multiply(BigDecimal.valueOf(0.7778)).multiply(ironRecoveryRate);
        BigDecimal siCSubtract = BigDecimal.valueOf(1).subtract(siliconContent).subtract(carbonContent);
        // 成品品味
        BigDecimal endProductTaste = cr.divide(
                cr.add(fe).divide(siCSubtract, 20, RoundingMode.HALF_UP)
                        .multiply(BigDecimal.valueOf(100))
                , 20, RoundingMode.HALF_UP);
        resultDTO.setEndProductTaste(endProductTaste);
        /**-------------------碳耗----------------------*/
        //  {[Cr2O3*0.684*铬回收率*36/104
        //  +FeO*0.7778*铁回收率*12/56
        //  +（Cr2O3*0.684*铬回收率+FeO*0.7778*铁回收率）/（1-C含量-SI含量）*Si含量*24/28  ---result1
        //  +（Cr2O3*0.684*铬回收率+FeO*0.7778*铁回收率）/（1-C含量-Si含量）*C含量]   -----result2
        //  /利用率/固定碳
        //  *[
        //  1000/（Cr2O3*0.684*铬回收率+FeO*0.7778*铁回收率）/（1-C含量-Si含量）  --- result3
        //  }/1000
        BigDecimal cr1 = cr.multiply(BigDecimal.valueOf(36)).divide(BigDecimal.valueOf(104), 20, RoundingMode.HALF_UP);
        BigDecimal fe1 = fe.multiply(BigDecimal.valueOf(12)).divide(BigDecimal.valueOf(56), 20, RoundingMode.HALF_UP);
        BigDecimal result1 = cr.add(fe).divide(siCSubtract.multiply(siliconContent)
                        .multiply(BigDecimal.valueOf(24))
                        .divide(BigDecimal.valueOf(28), 20, RoundingMode.HALF_UP)
                , 20, RoundingMode.HALF_UP);
        BigDecimal result2 = cr.add(fe).divide(siCSubtract.multiply(siliconContent), 20, RoundingMode.HALF_UP)
                .multiply(carbonContent);
        BigDecimal result3 = BigDecimal.valueOf(1000).divide(cr.add(fe), 20, RoundingMode.HALF_UP)
                .divide(siCSubtract, 20, RoundingMode.HALF_UP);
        // 碳耗
        BigDecimal carbonConsumption = cr1.add(fe1).add(result1).add(result2)
                .divide(costAllocation.getUtilizationRate(), 20, RoundingMode.HALF_UP)
                .divide(costAllocation.getFixedCarbon(), 20, RoundingMode.HALF_UP)
                .multiply(result3)
                .divide(BigDecimal.valueOf(1000), 20, RoundingMode.HALF_UP);
        resultDTO.setCarbonConsumption(carbonConsumption);
        /**-------------------矿耗----------------------*/
        // 成品品位/（Cr2O3*0.684*铬回收率）
        BigDecimal oreConsumption = endProductTaste.divide(cr, 20, RoundingMode.HALF_UP);
        resultDTO.setOreConsumption(oreConsumption);
        /**-------------------50矿耗----------------------*/
        // 矿耗/成品品位*50
        BigDecimal fiftyOreConsumption = oreConsumption.divide(endProductTaste, 20, RoundingMode.HALF_UP)
                .multiply(BigDecimal.valueOf(50));
        resultDTO.setFiftyOreConsumption(fiftyOreConsumption);
        /**-------------------矿成本----------------------*/
        // ∑料批*Cr回收率*     （（Cr2O3*现货单价）/原料进口税+平均运费/(1+运费税） ）*（1+运输损耗）
        // 这里的料批默认为1，运费税默认为9%，原料进口税默认为13%
        // 现货单价
        BigDecimal spotUnitPrice = rq.getSpotUnitPrice();
        BigDecimal miningCost = materialBatch.multiply(chromiumRecoveryRate)
                .multiply(chromiumTrioxide.multiply(spotUnitPrice)
                        .divide(rawMaterialImportTax, 20, RoundingMode.HALF_UP).add(rq.getFreight()
                                .divide(BigDecimal.valueOf(1).add(freightTax), 20, RoundingMode.HALF_UP)))
                .divide(BigDecimal.valueOf(1).add(costAllocation.getTransportLoss()), 20, RoundingMode.HALF_UP);
        resultDTO.setMiningCost(miningCost);
        /**-------------------碳成本----------------------*/
        //（兰碳单价*兰碳比例+焦炭比例*焦炭单价）*碳耗
        BigDecimal carbonCost = costAllocation.getSemiCokeUnitPrice().multiply(costAllocation.getSemiCokeRatio())
                .add(costAllocation.getCokeUnitPrice().multiply(costAllocation.getCokeRatio()))
                .multiply(carbonConsumption);
        resultDTO.setCarbonCost(carbonCost);
        /**-------------------电成本----------------------*/
        // 动力电耗*动力电单价+炉电单价*炉电吨耗
        BigDecimal electricityCost = costAllocation.getElectricityConsume().multiply(costAllocation.getElectricityUnitPrice())
                .add(costAllocation.getFurnaceElectricityTonConsume().multiply(costAllocation.getFurnaceElectricityUnitPrice()));
        resultDTO.setElectricityCost(electricityCost);
        /**-------------------辅料成本----------------------*/
        // 硅石吨耗*硅石单价+电极糊单价*电极糊吨耗
        BigDecimal accessoriesCost = costAllocation.getSilicaTonConsume().multiply(costAllocation.getSilicaUnitPrice())
                .add(costAllocation.getElectrodePasteTonConsume().multiply(costAllocation.getElectrodePasteUnitPrice()));
        resultDTO.setAccessoriesCost(accessoriesCost);
        /**-------------------三费----------------------*/
        // 管理费用+财务费用+销售费用
        BigDecimal threeCharges = costAllocation.getLabourService()
                .add(costAllocation.getManagementCost())
                .add(costAllocation.getFinanceCost());
        resultDTO.setThreeCharges(threeCharges);
        /**-------------------制造成本----------------------*/
        // 制造费用+辅料易耗+直接人工+劳务
        BigDecimal manufacturingCost = costAllocation.getManufacturingCost()
                .add(costAllocation.getConsumableAccessorie())
                .add(costAllocation.getDirectLabor())
                .add(costAllocation.getLabourService());
        resultDTO.setManufacturingCost(manufacturingCost);
        /**-------------------完全成本----------------------*/
        // 矿成本+电成本+碳成本+辅材成本+其制造成本+三费
        BigDecimal fullCost = miningCost.add(electricityCost).add(carbonCost)
                .add(accessoriesCost).add(manufacturingCost).add(threeCharges);
        resultDTO.setFullCost(fullCost);
        /**-------------------50完全成本----------------------*/
        // 完全成本/成品品位*50
        BigDecimal fiftyFullCost = fullCost.divide(endProductTaste, 20, RoundingMode.HALF_UP)
                .multiply(BigDecimal.valueOf(50));
        resultDTO.setFiftyFullCost(fiftyFullCost);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, resultDTO);
    }
}
