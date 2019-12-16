package com.bee.platform.user.controller;


import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.bee.platform.common.dto.IndustryDTO;
import com.bee.platform.common.entity.IndustryInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.user.entity.Industry;
import com.bee.platform.user.rq.IndustryRQ;
import com.bee.platform.user.service.IndustryService;
import com.google.common.collect.Lists;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.*;
import springfox.documentation.annotations.ApiIgnore;

import javax.validation.Valid;
import java.util.List;
import java.util.stream.Collectors;

/**
 * <p>
 * 行业分类信息表 前端控制器
 * </p>
 *
 * @author cheng.ke
 * @since 2019-04-24
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "industry", tags = "行业相关接口")
@RequestMapping("/industry")
public class IndustryController {

    @Autowired
    private IndustryService industryService;

    @ApiOperation(value = "查询所有行业信息",notes = "查询所有行业信息")
    @GetMapping("/getAllIndustry")
    public ResponseResult<List<List<Industry>>> getAllIndustry(){

        List<Industry> industries = industryService.selectList(new EntityWrapper<Industry>().eq("level", 1));
        List<Integer> ids = industries.stream().map(Industry::getId).collect(Collectors.toList());
        List<List<Industry>> lists =Lists.newArrayList();
        ids.forEach(id->{
            List<Industry> list = Lists.newArrayList();
            Industry parent = industryService.selectById(id);
            list.add(parent);
            List<Industry> childList = industryService.selectList(new EntityWrapper<Industry>().eq("pid", id));
            list.addAll(childList);
            lists.add(list);
        });

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, lists);

    }

    @ApiOperation(value = "查询父级id相同的行业",notes = "查询父级id相同的行业")
    @GetMapping("/getIndustryByParentId/{pid}")
    public ResponseResult<List<Industry>> getIndustryByParentId(@PathVariable("pid")int pid){
        if(ObjectUtils.isEmpty(pid)){
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.PARAMETER_INCOMPLETE);
        }
        List<Industry> industries = industryService.getIndustryByParentId(pid);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, industries);
    }

    @ApiOperation(value = "根据id查询行业详细信息" ,notes = "根据id查询行业详细信息")
    @GetMapping("/getAllIndustryById/{id}")
    public ResponseResult<IndustryDTO> getAllIndustryById(@PathVariable("id")int id){
        if(ObjectUtils.isEmpty(id)){
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.PARAMETER_INCOMPLETE);
        }
            IndustryDTO industryDTO =  industryService.selectIndustry(String.valueOf(id));

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, industryDTO);
    }

    @ApiOperation(value = "根据id查询行业信息" ,notes = "根据id查询行业信息")
    @GetMapping("/getIndustryById/{id}")
    public ResponseResult<IndustryInfo> getIndustryById(@PathVariable("id")int id){
        if(ObjectUtils.isEmpty(id)){
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.PARAMETER_INCOMPLETE);
        }
        IndustryInfo industry = industryService.getIndustryById(id);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,industry);
    }

    @ApiOperation(value ="添加行业信息",notes = "添加行业信息")
    @ApiIgnore
    @PostMapping("/addIndustry")
    public ResponseResult addIndustry(@RequestBody() @Valid IndustryRQ rq, BindingResult bindingResult){
       if(ObjectUtils.isEmpty(rq) || bindingResult.hasErrors() ){
           log.error("参数异常");
           return ResponseResult.buildResponseResult(ResCodeEnum.PARAMETER_INCOMPLETE);
       }

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,industryService.addIndustry(rq));
    }

    @ApiOperation(value = "修改行业信息",notes ="修改行业信息")
    @ApiIgnore
    @PutMapping("/updateIndustry")
    public ResponseResult updateIndustry(@RequestBody() @Valid IndustryRQ rq, BindingResult bindingResult){

        if(ObjectUtils.isEmpty(rq)||ObjectUtils.isEmpty(rq.getId())||bindingResult.hasErrors()){
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.PARAMETER_INCOMPLETE);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,industryService.updateIndustry(rq));
    }

}

