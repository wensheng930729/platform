package com.bee.platform.costcontrol.controller;


import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.UserInfoUtils;
import com.bee.platform.common.utils.WebUtils;
import com.bee.platform.costcontrol.service.ErpCostSimulationService;
import com.bee.platform.costcontroller.dto.ErpCostSimulationComputedResultDTO;
import com.bee.platform.costcontroller.dto.ErpCostSimulationDetailDTO;
import com.bee.platform.costcontroller.dto.ErpCostSimulationQueryDTO;
import com.bee.platform.costcontroller.rq.ErpCostSimulationAddRQ;
import com.bee.platform.costcontroller.rq.ErpCostSimulationCalculationRQ;
import com.bee.platform.costcontroller.rq.ErpCostSimulationQueryRQ;
import com.bee.platform.costcontroller.rq.ErpCostSimulationUpdateRQ;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import javax.validation.Valid;
import java.util.List;

/**
 * <p>
 * 成本模拟基础配置 前端控制器
 * </p>
 *
 * @author liliang123
 * @since 2019-06-25
 */
@RestController
@RequestMapping("/erpCostSimulation")
@Api(value = "ErpCostSimulationController", tags = "erp-成本模拟相关接口")
public class ErpCostSimulationController {

    @Autowired
    private ErpCostSimulationService costSimulationService;
    @Autowired
    private UserInfoUtils userInfoUtils;

    @PostMapping("/add")
    @ApiOperation(value = "新增")
    public ResponseResult addCostSimulation(HttpServletRequest request, @RequestBody @Valid ErpCostSimulationAddRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return costSimulationService.addCostSimulation(rq, userInfo);
    }

    @GetMapping("/delete/{id}")
    @ApiOperation(value = "删除")
    public ResponseResult deleteCostSimulation(HttpServletRequest request, @PathVariable("id") Integer id) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return costSimulationService.deleteCostSimulation(id);
    }

    @PostMapping("/update")
    @ApiOperation(value = "修改")
    public ResponseResult updateCostSimulation(HttpServletRequest request, @RequestBody @Valid ErpCostSimulationUpdateRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return costSimulationService.updateCostSimulation(rq, userInfo);
    }

    @PostMapping("/list")
    @ApiOperation(value = "列表")
    public ResponseResult<List<ErpCostSimulationQueryDTO>> getList(HttpServletRequest request, @RequestBody @Valid ErpCostSimulationQueryRQ rq, Page page) {
        // 从header获取公司id
        Integer companyId = Integer.valueOf(WebUtils.getParam(ConstantsUtil.COMPANY, request));
        if (ObjectUtils.isEmpty(companyId)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return costSimulationService.getSimulationList(companyId,rq, userInfo, page);
    }

    @GetMapping("/detail/{id}")
    @ApiOperation(value = "单个详情")
    public ResponseResult<ErpCostSimulationDetailDTO> getCostSimulationDetail(HttpServletRequest request, @PathVariable("id") Integer id) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return costSimulationService.getCostSimulationDetail(id);
    }

}

