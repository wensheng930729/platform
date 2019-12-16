package com.bee.platform.user.service.impl;

import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.constants.enums.EnumComonRegionLevel;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.user.dao.mapper.CommonRegionMapper;
import com.bee.platform.user.entity.CommonRegion;
import com.bee.platform.user.service.CommonRegionService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.util.ObjectUtils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * <p>
 * 全国地区表 服务实现类
 * </p>
 *
 * @author junyang.li123
 * @since 2019-03-05
 */
@Slf4j
@Service
public class CommonRegionServiceImpl extends ServiceImpl<CommonRegionMapper, CommonRegion> implements CommonRegionService {

    @Override
    public ResponseResult<Map<String, Object>> findAllRegionById(Integer id) {
        try {
            //获取到区级信息
            CommonRegion county = selectById(id);
            // 如果根据id查询到的不是区县级，则不进行操作
            if (ObjectUtils.isEmpty(county) || !EnumComonRegionLevel.level.four.getKey().equals(county.getLevel())) {
                return ResponseResult.fail("查询失败");
            }
            CommonRegion city = null;
            CommonRegion province = null;
            if (!ObjectUtils.isEmpty(county)) {
                //获取到市级信息
                city = selectById(county.getPid());
            }
            if (!ObjectUtils.isEmpty(city)) {
                //获取到省级信息
                province = selectById(city.getPid());
            }
            Map<String, Object> returnMap = new HashMap<String, Object>();
            returnMap.put("province", province);
            returnMap.put("city", city);
            returnMap.put("county", county);
            return ResponseResult.success(returnMap);
        } catch (Exception e) {
            log.error("查询地址异常", e);
        }
        return ResponseResult.fail("查询失败");
    }

    @Override
    public ResponseResult<List<Integer>> getProvinceCityId(Integer id) {
        //获取到区级信息
        CommonRegion county = selectById(id);
        // 如果根据id查询到的不是区县级，则不进行操作
        if (!EnumComonRegionLevel.level.four.getKey().equals(county.getLevel())) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED);
        }
        //获取到市级信息
        CommonRegion city = selectById(county.getPid());
        //获取到省级信息
        CommonRegion province = selectById(city.getPid());
        ArrayList<Integer> list = new ArrayList<>(2);
        // 添加省级id
        list.add(province == null ? null : city.getId());
        // 添加市级id
        list.add(city.getId());
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, list);
    }
}
