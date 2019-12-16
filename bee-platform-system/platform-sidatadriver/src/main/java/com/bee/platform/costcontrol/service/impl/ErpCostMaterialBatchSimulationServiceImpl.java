package com.bee.platform.costcontrol.service.impl;

import com.alibaba.fastjson.JSON;
import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.costcontrol.dao.mapper.ErpCostMaterialBatchSimulationMapper;
import com.bee.platform.costcontrol.entity.ErpCostMaterialBatchSimulation;
import com.bee.platform.costcontrol.entity.ErpCostMaterialSimulationElement;
import com.bee.platform.costcontrol.entity.ErpCostSimulationResult;
import com.bee.platform.costcontrol.service.ErpCostMaterialBatchSimulationService;
import com.bee.platform.costcontrol.service.ErpCostMaterialSimulationElementService;
import com.bee.platform.costcontrol.service.ErpCostSimulationResultService;
import com.bee.platform.costcontroller.dto.*;
import com.bee.platform.costcontroller.rq.*;
import com.bee.platform.user.authority.dto.AuthEnterpriseFlatDTO;
import com.bee.platform.user.service.feign.AuthEnterpriseFeignClient;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Objects;

/**
 * <p>
 * 料批模拟 服务实现类
 * </p>
 *
 * @author xin.huang
 * @since 2019-06-25
 */
@Service
public class ErpCostMaterialBatchSimulationServiceImpl extends ServiceImpl<ErpCostMaterialBatchSimulationMapper, ErpCostMaterialBatchSimulation> implements ErpCostMaterialBatchSimulationService {

    @Autowired
    private ErpCostMaterialSimulationElementService elementService;

    @Autowired
    private ErpCostSimulationResultService resultService;

    @Autowired
    private ErpCostMaterialBatchSimulationMapper materialBatchSimulationMapper;

    @Autowired
    private AuthEnterpriseFeignClient authEnterpriseFeignClient;

    /**
     * 新增料批模拟
     * @param rq
     * @return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> add(AuthPlatformUserInfo userInfo, ErpCostMaterialSimulationRQ rq) {
        //判断计算结果中已确认的条数
        if (!CollectionUtils.isEmpty(rq.getResultList())) {
            int countConfirm = 0;
            for (ErpCostMaterialSimulationResultRQ result : rq.getResultList()) {
                if (result.getStatus().equals(Status.TRUE.getKey())) {
                    countConfirm = countConfirm + 1;
                }
            }
            if (countConfirm > 1) {
                return ResponseResult.buildResponseResult(ResCodeEnum.ERP_COST_SIMULATION_RESULT_ONLY);
            }
        }
        ErpCostMaterialBatchSimulation erpCostMaterialBatchSimulation = BeanUtils.copyProperties(rq, ErpCostMaterialBatchSimulation.class);
        erpCostMaterialBatchSimulation.setCreator(userInfo.getId()).setCreateTime(new Date()).setUpdateTime(new Date());
        if (Objects.nonNull(rq.getAllocation())) {
            String json = JSON.toJSONString(rq.getAllocation());
            erpCostMaterialBatchSimulation.setAllocationInfo(json);
        }
        //添加料批基本配置
        insert(erpCostMaterialBatchSimulation);

        //添加料批模拟原料成分
        if (!CollectionUtils.isEmpty(rq.getElementList())) {
            List<ErpCostMaterialSimulationElement> elementList = BeanUtils.assemble(ErpCostMaterialSimulationElement.class, rq.getElementList());
            elementList.forEach(e -> {
                e.setSimulationId(erpCostMaterialBatchSimulation.getId())
                 .setCreateTime(new Date())
                 .setUpdateTime(new Date());
            });
            elementService.insertBatch(elementList);
        }

        //添加料批模拟计算结果
        if (!CollectionUtils.isEmpty(rq.getResultList())) {
            List<ErpCostSimulationResult> resultList = BeanUtils.assemble(ErpCostSimulationResult.class, rq.getResultList());
            resultList.forEach(result -> {
                result.setSimulationId(erpCostMaterialBatchSimulation.getId()).setCreateTime(new Date()).setUpdateTime(new Date());
            });
            resultService.insertBatch(resultList);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * 编辑料批模拟
     * @param userInfo
     * @param rq
     * @return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> update(AuthPlatformUserInfo userInfo, ErpCostMaterialSimulationRQ rq) {
        if (Objects.nonNull(rq) && Objects.nonNull(rq.getId())) {
            ErpCostMaterialBatchSimulation erpCostMaterialBatchSimulation = selectOne(new EntityWrapper<>(new ErpCostMaterialBatchSimulation()
                    .setId(rq.getId()).setDeleted(Status.FALSE.getKey())));
            if (Objects.isNull(erpCostMaterialBatchSimulation)) {
                return ResponseResult.buildResponseResult(ResCodeEnum.NO_DATA);
            }
        }

        //判断计算结果中已确认的条数
        if (!CollectionUtils.isEmpty(rq.getResultList())) {
            int countConfirm = 0;
            for (ErpCostMaterialSimulationResultRQ result : rq.getResultList()) {
                if (result.getStatus().equals(Status.TRUE.getKey())) {
                    countConfirm = countConfirm + 1;
                }
            }
            if (countConfirm > 1) {
                return ResponseResult.buildResponseResult(ResCodeEnum.ERP_COST_SIMULATION_RESULT_ONLY);
            }
        }

        ErpCostMaterialBatchSimulation erpCostMaterialBatchSimulation = BeanUtils.copyProperties(rq, ErpCostMaterialBatchSimulation.class);
        erpCostMaterialBatchSimulation.setCreator(userInfo.getId()).setUpdateTime(new Date());
        if (Objects.nonNull(rq.getAllocation())) {
            String json = JSON.toJSONString(rq.getAllocation());
            erpCostMaterialBatchSimulation.setAllocationInfo(json);
        }
        //编辑料批基本配置
        updateById(erpCostMaterialBatchSimulation);

        //删除料批模拟原料成分
        elementService.update(new ErpCostMaterialSimulationElement().setDeleted(Status.TRUE.getKey()),
                new EntityWrapper<>(new ErpCostMaterialSimulationElement().setSimulationId(rq.getId())));
        //添加料批模拟原料成分
        if (!CollectionUtils.isEmpty(rq.getElementList())) {
            List<ErpCostMaterialSimulationElement> elementList = BeanUtils.assemble(ErpCostMaterialSimulationElement.class, rq.getElementList());
            elementList.forEach(e -> {
                e.setSimulationId(erpCostMaterialBatchSimulation.getId())
                        .setCreateTime(new Date())
                        .setUpdateTime(new Date());
            });
            elementService.insertBatch(elementList);
        }

        //删除料批模拟计算结果
        resultService.update(new ErpCostSimulationResult().setDeleted(Status.TRUE.getKey()),
                new EntityWrapper<>(new ErpCostSimulationResult().setSimulationId(rq.getId())));
        //添加料批模拟计算结果
        if (!CollectionUtils.isEmpty(rq.getResultList())) {
            List<ErpCostSimulationResult> resultList = BeanUtils.assemble(ErpCostSimulationResult.class, rq.getResultList());
            resultList.forEach(result -> {
                result.setSimulationId(erpCostMaterialBatchSimulation.getId()).setCreateTime(new Date()).setUpdateTime(new Date());
            });
            resultService.insertBatch(resultList);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * 更新计算结果状态
     * @param userInfo
     * @param rq
     * @return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> updateStatus(AuthPlatformUserInfo userInfo, ErpCostSimulationResultStatusRQ rq) {
        //若是确认计算结果，则判断是否存在已确认的数据
        if (rq.getStatus().equals(Status.TRUE.getKey())) {
            int status = resultService.selectCount(new EntityWrapper<>(new ErpCostSimulationResult()
                    .setSimulationId(rq.getSimulationId()).setStatus(Status.TRUE.getKey()).setDeleted(Status.FALSE.getKey())));
            if (status > 0) {
                return ResponseResult.buildResponseResult(ResCodeEnum.ERP_COST_SIMULATION_RESULT_EXIT);
            }
        }

        ErpCostSimulationResult erpCostSimulationResult = resultService
                .selectOne(new EntityWrapper<>(new ErpCostSimulationResult()
                        .setId(rq.getId())
                        .setDeleted(Status.FALSE.getKey())));
        if (Objects.isNull(erpCostSimulationResult)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NO_DATA);
        }
        resultService.updateById(new ErpCostSimulationResult().setId(rq.getId()).setStatus(rq.getStatus()));
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * 删除料批模拟
     * @param id
     * @return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> deleteMaterial(Integer id) {
        ErpCostMaterialBatchSimulation erpCostMaterialBatchSimulation = selectOne(new EntityWrapper<>(new ErpCostMaterialBatchSimulation()
                .setId(id).setDeleted(Status.FALSE.getKey())));
        if (Objects.isNull(erpCostMaterialBatchSimulation)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NO_DATA);
        }
        updateById(new ErpCostMaterialBatchSimulation().setId(id).setDeleted(Status.TRUE.getKey()));
        //删除原料模拟
        elementService.update(new ErpCostMaterialSimulationElement().setDeleted(Status.TRUE.getKey()),
                new EntityWrapper<>(new ErpCostMaterialSimulationElement().setSimulationId(id)));
        //删除计算结果
        resultService.update(new ErpCostSimulationResult().setDeleted(Status.TRUE.getKey()),
                new EntityWrapper<>(new ErpCostSimulationResult().setSimulationId(id)));
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * 根据公司id查询料批模拟列表
     * @param companyId
     * @return
     */
    @Override
    public ResponseResult<List<ErpCostMaterialSimulationDTO>> findListByCompanyId(Integer companyId) {
        List<ErpCostMaterialBatchSimulation> erpCostMaterialBatchSimulations = selectList(new EntityWrapper<>(new ErpCostMaterialBatchSimulation()
                .setCompanyId(companyId).setDeleted(Status.FALSE.getKey())));
        List<ErpCostMaterialSimulationDTO> simulationList = BeanUtils.assemble(ErpCostMaterialSimulationDTO.class, erpCostMaterialBatchSimulations);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, simulationList);
    }

    /**
     * 条件查询料批模拟列表
     * @param rq
     * @return
     */
    @Override
    public ResponseResult<List<ErpCostMaterialSimulationListDTO>> findList(Integer companyId, ErpCostMaterialSimulationListRQ rq, Pagination pagination) {
        if (Objects.isNull(rq.getCompanyId())) {
            List<AuthEnterpriseFlatDTO> enterprises = authEnterpriseFeignClient
                    .getEnterpriseFlatByCompanyId(companyId).getObject();
            if (!CollectionUtils.isEmpty(enterprises)) {
                List<Integer> enterpriseIds = new ArrayList<Integer>();
                enterprises.forEach(enterprise -> {
                    enterpriseIds.add(enterprise.getValue());
                });
                rq.setEnterpriseIdList(enterpriseIds);
            }
        }
        List<ErpCostMaterialSimulationListDTO> simulationList = materialBatchSimulationMapper.findList(rq, pagination);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, simulationList, PageUtils.transToPage(pagination));
    }

    /**
     * 料批模拟详情
     * @param id
     * @return
     */
    @Override
    public ResponseResult<ErpCostMaterialSimulationDetailDTO> findInfo(Integer id) {
        ErpCostMaterialSimulationDetailDTO materialInfo = materialBatchSimulationMapper.findInfo(id);
        if (Objects.isNull(materialInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NO_DATA);
        }
        ErpCostMaterialSimulationDetailDTO erpCostMaterialSimulationDetailDTO = BeanUtils.copyProperties(materialInfo,
                ErpCostMaterialSimulationDetailDTO.class);
        //解析成本配置信息
        if (StringUtils.isNotBlank(erpCostMaterialSimulationDetailDTO.getAllocationInfo())) {
            ErpCostAllocationDetailDTO erpCostAllocationDetailDTO = JSON.parseObject(erpCostMaterialSimulationDetailDTO.getAllocationInfo(), ErpCostAllocationDetailDTO.class);
            erpCostMaterialSimulationDetailDTO.setAllocation(erpCostAllocationDetailDTO);
        }

        //查询原料成分信息
        List<ErpCostMaterialSimulationElement> elements = elementService.selectList(new EntityWrapper<>(new ErpCostMaterialSimulationElement()
                .setSimulationId(id).setDeleted(Status.FALSE.getKey())));
        if (!CollectionUtils.isEmpty(elements)) {
            List<ErpCostMaterialSimulationElementDTO> elementList = BeanUtils.assemble(ErpCostMaterialSimulationElementDTO.class, elements);
            ErpCostMaterialSimulationElementDTO e = new ErpCostMaterialSimulationElementDTO();
            e.setMaterialName("入炉综合值");
            BigDecimal chromiumTrioxide = BigDecimal.ZERO;
            BigDecimal ironOxide = BigDecimal.ZERO;
            BigDecimal magnesia = BigDecimal.ZERO;
            BigDecimal aluminumOxide = BigDecimal.ZERO;
            BigDecimal chromiumIronRatio = BigDecimal.ZERO;
            BigDecimal magnesiumAluminumRatio = BigDecimal.ZERO;
            for (ErpCostMaterialSimulationElementDTO element : elementList) {
                if (Objects.isNull(element.getMaterialRatio()) || element.getMaterialRatio().compareTo(BigDecimal.ZERO) == 0) {
                    continue;
                }
                if (Objects.nonNull(element.getChromiumTrioxide())) {
                    chromiumTrioxide = chromiumTrioxide.add(element.getChromiumTrioxide().multiply(element.getMaterialRatio()));
                }
                if (Objects.nonNull(element.getIronOxide())) {
                    ironOxide = ironOxide.add(element.getIronOxide().multiply(element.getMaterialRatio()));
                }
                if (Objects.nonNull(element.getMagnesia())) {
                    magnesia = magnesia.add(element.getMagnesia().multiply(element.getMaterialRatio()));
                }
                if (Objects.nonNull(element.getAluminumOxide())) {
                    aluminumOxide = aluminumOxide.add(element.getAluminumOxide().multiply(element.getMaterialRatio()));
                }
            }
            if (ironOxide.compareTo(BigDecimal.ZERO) == 1) {
                chromiumIronRatio = chromiumTrioxide.divide(ironOxide, 2, BigDecimal.ROUND_HALF_UP)
                        .multiply(BigDecimal.valueOf(0.6842))
                        .divide(BigDecimal.valueOf(0.7778), 2, BigDecimal.ROUND_HALF_UP);
            }
            if (aluminumOxide.compareTo(BigDecimal.ZERO) == 1) {
                magnesiumAluminumRatio = magnesia.divide(aluminumOxide, 2, BigDecimal.ROUND_HALF_UP);
            }
            e.setChromiumTrioxide(chromiumTrioxide).setIronOxide(ironOxide).setMagnesia(magnesia)
                    .setAluminumOxide(aluminumOxide).setChromiumIronRatio(chromiumIronRatio)
                    .setMagnesiumAluminumRatio(magnesiumAluminumRatio);
            erpCostMaterialSimulationDetailDTO.setStatistic(e);
            erpCostMaterialSimulationDetailDTO.setElementList(elementList);
        }
        //查询计算结果信息
        List<ErpCostSimulationResult> erpCostSimulationResults = resultService.selectList(new EntityWrapper<>(new ErpCostSimulationResult()
                .setSimulationId(id).setDeleted(Status.FALSE.getKey())));
        erpCostMaterialSimulationDetailDTO.setResultList(BeanUtils.assemble(ErpCostMaterialSimulationResultDTO.class, erpCostSimulationResults));
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, erpCostMaterialSimulationDetailDTO);
    }

    /**
     * 计算料批模拟结果
     * @param rq
     * @return
     */
    @Override
    public ResponseResult<ErpCostMaterialSimulationResultDTO> calculateResult(ErpCostMaterialSimulationRQ rq) {
        if (Objects.isNull(rq.getAllocation()) || CollectionUtils.isEmpty(rq.getElementList())) {
            return null;
        }
        ErpCostAllocationAddRQ allocation = rq.getAllocation();
        //计算入炉综合值
        BigDecimal chromiumTrioxide = BigDecimal.ZERO;
        BigDecimal ironOxide = BigDecimal.ZERO;
        BigDecimal magnesia = BigDecimal.ZERO;
        BigDecimal aluminumOxide = BigDecimal.ZERO;
        BigDecimal chromiumIronRatio = BigDecimal.ZERO;
        BigDecimal magnesiumAluminumRatio = BigDecimal.ZERO;

        //矿成本
        BigDecimal oreCost = BigDecimal.ZERO;

        ErpCostMaterialSimulationResultDTO result = new ErpCostMaterialSimulationResultDTO();
        //料批详情
        StringBuffer materialDetail = new StringBuffer();
        for (ErpCostMaterialSimulationElementRQ element : rq.getElementList()) {
            if (Objects.isNull(element.getMaterialRatio()) || element.getMaterialRatio().compareTo(BigDecimal.ZERO) == 0) {
                continue;
            }
            oreCost = element.getMaterialRatio().multiply(allocation.getChromiumRecoveryRate()).multiply(element.getPrice()).add(oreCost);

            materialDetail.append(element.getMaterialName()).append(element.getMaterialRatio()).append(",");
            if (Objects.nonNull(element.getChromiumTrioxide())) {
                chromiumTrioxide = chromiumTrioxide.add(element.getChromiumTrioxide().multiply(element.getMaterialRatio()));
            }
            if (Objects.nonNull(element.getIronOxide())) {
                ironOxide = ironOxide.add(element.getIronOxide().multiply(element.getMaterialRatio()));
            }
            if (Objects.nonNull(element.getMagnesia())) {
                magnesia = magnesia.add(element.getMagnesia().multiply(element.getMaterialRatio()));
            }
            if (Objects.nonNull(element.getAluminumOxide())) {
                aluminumOxide = aluminumOxide.add(element.getAluminumOxide().multiply(element.getMaterialRatio()));
            }
            if (Objects.nonNull(element.getChromiumIronRatio())) {
                chromiumIronRatio = chromiumIronRatio.add(element.getChromiumIronRatio().multiply(element.getMaterialRatio()));
            }
            if (Objects.nonNull(element.getMagnesiumAluminumRatio())) {
                magnesiumAluminumRatio = magnesiumAluminumRatio.add(element.getMagnesiumAluminumRatio().multiply(element.getMaterialRatio()));
            }
        }

        //计算结果
        String materialDetailStr = materialDetail.toString();
        materialDetailStr = materialDetailStr.substring(0, materialDetailStr.length() - 1);
        result.setMaterialDetail(materialDetailStr);
        //理论成品品位
        BigDecimal grade = BigDecimal.ZERO;
        BigDecimal g1 = chromiumTrioxide.multiply(BigDecimal.valueOf(0.684)).multiply(allocation.getChromiumRecoveryRate());
        BigDecimal g2 = ironOxide.multiply(BigDecimal.valueOf(0.7778)).multiply(allocation.getIronRecoveryRate());
        BigDecimal g3 = g1.add(g2);
        BigDecimal g4 = BigDecimal.ONE.subtract(allocation.getCarbonContent()).subtract(allocation.getSiliconContent());
        if (g3.compareTo(BigDecimal.ZERO) == 1 && g4.compareTo(BigDecimal.ZERO) == 1) {
            g3 = g3.divide(g4, 2, BigDecimal.ROUND_HALF_UP);
            grade = g1.divide(g3, 2, BigDecimal.ROUND_HALF_UP).multiply(BigDecimal.valueOf(100));
        }
        //料批矿耗
        BigDecimal materialMineCost = BigDecimal.ZERO;
        //50矿耗
        BigDecimal fiftyMineCost = BigDecimal.ZERO;
        if (g1.compareTo(BigDecimal.ZERO) == 1) {
            materialMineCost = grade.divide(g1, 2, BigDecimal.ROUND_HALF_UP);
        }
        if (grade.compareTo(BigDecimal.ZERO) == 1) {
            fiftyMineCost = materialMineCost.divide(grade, 2, BigDecimal.ROUND_HALF_UP).multiply(BigDecimal.valueOf(50));
        }
        //碳耗
        BigDecimal carbonCost = BigDecimal.ZERO;
        BigDecimal c1 = g1.multiply(BigDecimal.valueOf(36)).divide(BigDecimal.valueOf(104), 2, BigDecimal.ROUND_HALF_UP);
        BigDecimal c2 = g2.multiply(BigDecimal.valueOf(12)).divide(BigDecimal.valueOf(56), 2, BigDecimal.ROUND_HALF_UP);
        BigDecimal c3 = BigDecimal.ZERO;
        BigDecimal c4 = BigDecimal.ZERO;
        BigDecimal c5 = BigDecimal.ZERO;
        if (g4.compareTo(BigDecimal.ZERO) == 1) {
            c3 = g1.add(g2).divide(g4, 2, BigDecimal.ROUND_HALF_UP).multiply(allocation.getSiliconContent())
                    .multiply(BigDecimal.valueOf(24)).divide(BigDecimal.valueOf(28), 2, BigDecimal.ROUND_HALF_UP);
            c4 = g1.add(g2).divide(g4, 2, BigDecimal.ROUND_HALF_UP).multiply(allocation.getCarbonContent());
            c5 = g1.add(g2).divide(g4, 2, BigDecimal.ROUND_HALF_UP);
        }
        BigDecimal c14 = c1.add(c2).add(c3).add(c4);
        if (allocation.getUtilizationRate().compareTo(BigDecimal.ZERO) == 1
                && allocation.getFixedCarbon().compareTo(BigDecimal.ZERO) == 1
                && g4.compareTo(BigDecimal.ZERO) == 1) {
            carbonCost = c14.divide(allocation.getUtilizationRate(), 2, BigDecimal.ROUND_HALF_UP)
                    .divide(allocation.getFixedCarbon(), 2, BigDecimal.ROUND_HALF_UP)
                    .multiply(BigDecimal.valueOf(1000)).divide(c5, 2, BigDecimal.ROUND_HALF_UP)
                    .divide(BigDecimal.valueOf(1000), 2, BigDecimal.ROUND_HALF_UP);
        }

        //电成本
        BigDecimal electricity = allocation.getElectricityConsume().multiply(allocation.getElectricityUnitPrice())
                .add(allocation.getFurnaceElectricityUnitPrice().multiply(allocation.getFurnaceElectricityTonConsume()));
        //碳成本
        BigDecimal cCost = allocation.getSemiCokeUnitPrice().multiply(allocation.getSemiCokeRatio())
                .add(allocation.getCokeRatio().multiply(allocation.getCokeUnitPrice())).multiply(carbonCost);
        //辅材成本
        BigDecimal auxiliaryCost = allocation.getSilicaTonConsume().multiply(allocation.getSilicaUnitPrice())
                .add(allocation.getElectrodePasteUnitPrice().multiply(allocation.getElectrodePasteTonConsume()));
        //制造成本
        BigDecimal productCost = allocation.getManufacturingCost().add(allocation.getConsumableAccessorie())
                .add(allocation.getDirectLabor()).add(allocation.getLabourService());
        //三费
        BigDecimal threeCost = allocation.getManagementCost().add(allocation.getFinanceCost()).add(allocation.getSaleCost());

        //完全成本
        BigDecimal fullCost = oreCost.add(electricity).add(cCost).add(auxiliaryCost).add(productCost).add(threeCost);

        //50完全成本
        BigDecimal fiftyFullCost = BigDecimal.ZERO;
        if (grade.compareTo(BigDecimal.ZERO) == 1) {
            fiftyFullCost = fullCost.divide(grade, 2, BigDecimal.ROUND_HALF_UP).multiply(BigDecimal.valueOf(50));
        }
        result.setGrade(grade).setCarbonCost(carbonCost).setFiftyFullCost(fiftyFullCost)
                .setFiftyMineCost(fiftyMineCost).setMaterialMineCost(materialMineCost);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, result);
    }

}
