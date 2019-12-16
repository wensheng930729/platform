package com.bee.platform.user.controller;


import com.bee.platform.common.dto.RegionDTO;
import com.bee.platform.common.entity.Region;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.service.RegionService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * <p>
 * 全国地区表 前端控制器
 * </p>
 *
 * @author junyang.li123
 * @since 2019-03-05
 */
@RestController
@CrossOrigin(origins = "*")
@Api(value = "region", tags = "地区相关接口")
@RequestMapping("/api/user/region")
public class CommonRegionController {

    @Autowired
    private RegionService regionService;

    @ApiOperation(value = "查询父级id相同的地区")
    @RequestMapping(value = "/findRegionByParentId/{pid}", method = RequestMethod.GET)
    public ResponseResult findRegionByParentId(@PathVariable int pid) {
        List<Region> regions = regionService.findRegionByParentId(pid);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, regions);
    }

    @ApiOperation(value = "根据id查询地区详细信息")
    @RequestMapping(value = "/findAllRegionById/{id}", method = RequestMethod.GET)
    public ResponseResult findAllRegionById(@PathVariable int id) {
        RegionDTO regionDTO = regionService.selectRegion(String.valueOf(id));
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, regionDTO);
    }


    @ApiOperation(value = "查询所有地区信息",notes = "查询所有地区信息")
    @GetMapping(value = "/getAllRegion")
    public ResponseResult getAllRegion(){

        return  ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, regionService.getAllRegion());
    }

    @ApiOperation(value = "查询所有的省级",notes = "查询所有的省级")
    @GetMapping(value = "/getAllProvince")
    public ResponseResult<List<Region>> getAllProvince(){
        return  ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, regionService.getAllProvince());
    }


}

