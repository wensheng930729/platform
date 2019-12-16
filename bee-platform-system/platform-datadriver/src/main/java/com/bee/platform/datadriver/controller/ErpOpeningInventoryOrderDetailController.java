package com.bee.platform.datadriver.controller;

import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.UserInfoUtils;
import com.bee.platform.datadriver.dto.ErpOpeningInventoryOrderDetailDTO;
import com.bee.platform.datadriver.entity.ErpOpeningInventoryOrderDetail;
import com.bee.platform.datadriver.rq.ErpOpeningInventoryOrderDetailRQ;
import com.bee.platform.datadriver.service.ErpOpeningInventoryOrderDetailService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.*;
import springfox.documentation.annotations.ApiIgnore;

import javax.servlet.http.HttpServletRequest;
import javax.validation.Valid;
import java.util.List;

/**
 * <p>
 * 期初库存明细表 前端控制器
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-28
 */

@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "erpOpeningInventoryOrderDetail", tags = "期初库存明细表相关接口")
@RequestMapping("/erpOpeningInventoryOrderDetail")
public class ErpOpeningInventoryOrderDetailController {

    @Autowired
    private UserInfoUtils userInfoUtils;


    @Autowired
    private ErpOpeningInventoryOrderDetailService inventoryOrderDetailService;

    @GetMapping("/listOpeningInventoryOrderDetail")
    @ApiOperation(value = "查询期初库存明细列表")
    @ApiIgnore
    public ResponseResult<List<ErpOpeningInventoryOrderDetail>> listOpeningInventoryOrderDetail(HttpServletRequest request, @RequestParam String id) {
        if (StringUtils.isEmpty(id)) {
            log.info("参数错误");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, inventoryOrderDetailService.listOpeningInventoryOrderDetail(id));
    }

    @PostMapping("/saveOpeningInventoryOrderDetail")
    @ApiOperation(value = "保存期初库存明细")
    public ResponseResult<Integer> saveOpeningInventoryOrderDetail(HttpServletRequest request, @RequestBody @Valid ErpOpeningInventoryOrderDetailRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo) || ObjectUtils.isEmpty(userInfo.getId())) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        if (ObjectUtils.isEmpty(rq)) {
            log.info("参数错误");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        Integer id = inventoryOrderDetailService.saveOpeningInventoryOrderDetail(userInfo, rq);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);
    }


    @ApiOperation(value = "根据id删除期初库存详情")
    @DeleteMapping("/deleteOpeningInventoryOrderDetailById/{id}")
    public ResponseResult<Integer> deleteOpeningInventoryOrderDetailById(HttpServletRequest request, @PathVariable Integer id) {
        if (ObjectUtils.isEmpty(id)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo) || ObjectUtils.isEmpty(userInfo.getId()) || ObjectUtils.isEmpty(userInfo.getOrgId())) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        inventoryOrderDetailService.deleteOpeningInventoryOrderDetailById(userInfo, id);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);
    }


    @ApiOperation(value = "根据id查询期初库存详情")
    @GetMapping("getOpeningInventoryOrderDetailById")
    public ResponseResult<ErpOpeningInventoryOrderDetailDTO> getOpeningInventoryOrderDetailById(@RequestParam Integer id) {

        if (ObjectUtils.isEmpty(id)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        ErpOpeningInventoryOrderDetailDTO dto = inventoryOrderDetailService.getOpeningInventoryOrderDetailById(id);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
    }


}

