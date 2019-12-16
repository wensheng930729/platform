package com.bee.platform.datadriver.controller;


import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.UserInfoUtils;
import com.bee.platform.common.utils.WebUtils;
import com.bee.platform.datadriver.dto.ErpStockCheckDTO;
import com.bee.platform.datadriver.dto.ErpStockCheckProductListDTO;
import com.bee.platform.datadriver.dto.ErpStockCheckSearchDTO;
import com.bee.platform.datadriver.enums.EnumBusinessType;
import com.bee.platform.datadriver.rq.ErpStockCheckRQ;
import com.bee.platform.datadriver.rq.ErpStockCheckSearchRQ;
import com.bee.platform.datadriver.service.ErpStockCheckService;
import com.bee.platform.datadriver.support.OperateType;
import com.bee.platform.datadriver.support.annotation.Log;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import javax.validation.Valid;
import java.util.List;

/**
 * <p>
 * 库存盘点主单表 前端控制器
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-28
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "erpStockCheck", tags = "库存盘点")
@RequestMapping("/erpStockCheck")
public class ErpStockCheckController {

    @Autowired
    private UserInfoUtils userInfoUtils;

    @Autowired
    private ErpStockCheckService erpStockCheckService;

    @ApiOperation(value = "查询企业下的库存商品信息")
    @GetMapping("/getProductListByCompanyId")
    public ResponseResult<List<ErpStockCheckProductListDTO>> getProductListByCompanyId(@RequestParam Integer companyId) {

        List<ErpStockCheckProductListDTO> dto = erpStockCheckService.getProductListByCompanyId(companyId);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
    }


    @ApiOperation(value = "条件搜索库存盘点")
    @PostMapping("/searchStockCheckByCondition")
    public ResponseResult<List<ErpStockCheckSearchDTO>> searchStockCheckByCondition(HttpServletRequest request, @RequestBody ErpStockCheckSearchRQ rq, Page page) {
        if (ObjectUtils.isEmpty(rq)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        Integer companyId = Integer.valueOf(WebUtils.getParam(ConstantsUtil.COMPANY, request));

        if (ObjectUtils.isEmpty(companyId) && ObjectUtils.isEmpty(rq.getCompanyId())){
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        return erpStockCheckService.searchStockCheckByCondition(rq, page, companyId);
    }


    @ApiOperation(value = "根据id查看库存盘点详情")
    @GetMapping("/getStockCheckById/{id}")
    public ResponseResult<ErpStockCheckDTO> getStockCheckById(@PathVariable Integer id) {
        if (ObjectUtils.isEmpty(id)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        ErpStockCheckDTO dto = erpStockCheckService.getStockCheckById(id);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);

    }


    @Log(businessType = EnumBusinessType.STOCK_CHECK, operateType = OperateType.DELETE)
    @ApiOperation(value = "删除库存盘点")
    @DeleteMapping("/deleteStockCheckById/{id}")
    public ResponseResult<Integer> deleteStockCheckById(HttpServletRequest request, @PathVariable Integer id) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo) || ObjectUtils.isEmpty(userInfo.getId())) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }

        erpStockCheckService.deleteStockCheckById(userInfo, id);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);
    }

    @Log(businessType = EnumBusinessType.STOCK_CHECK, operateType = OperateType.EDIT)
    @ApiOperation(value = "修改库存盘点单状态")
    @PutMapping("/updateStockCheckState")
    public ResponseResult<Integer> updateStockCheckState(HttpServletRequest request, @RequestParam("id") Integer id, @RequestParam("state") Integer state) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo) || ObjectUtils.isEmpty(userInfo.getId())) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }

        erpStockCheckService.updateStockCheckState(userInfo, id, state);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);
    }

    @Log(businessType = EnumBusinessType.STOCK_CHECK, operateType = OperateType.ADD)
    @ApiOperation(value = "保存库存盘点单")
    @PostMapping("/saveStockCheck")
    public ResponseResult<Integer> saveStockCheck(HttpServletRequest request, @RequestBody @Valid ErpStockCheckRQ rq) {
        if (ObjectUtils.isEmpty(rq)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo) || ObjectUtils.isEmpty(userInfo.getId()) || ObjectUtils.isEmpty(userInfo.getOrgId())) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }

        Integer id = erpStockCheckService.saveStockCheck(userInfo, rq);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);
    }


}

