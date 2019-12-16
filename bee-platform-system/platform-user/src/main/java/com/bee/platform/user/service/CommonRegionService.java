package com.bee.platform.user.service;

import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.user.entity.CommonRegion;

import java.util.List;
import java.util.Map;

import com.baomidou.mybatisplus.service.IService;

/**
 * <p>
 * 全国地区表 服务类
 * </p>
 *
 * @author junyang.li123
 * @since 2019-03-05
 */
public interface CommonRegionService extends IService<CommonRegion> {

    ResponseResult<Map<String,Object>> findAllRegionById(Integer id);

    /**
     * 根据区县地区id 获取省级和市级id
     * @param id
     * @return
     */
    ResponseResult<List<Integer>> getProvinceCityId(Integer id);
}
