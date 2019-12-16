package com.bee.platform.user.dao.mapper;


import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.business.dto.ArticlesDTO;
import com.bee.platform.user.entity.Articles;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Map;

/**
 * <p>
 *  Mapper 接口
 * </p>
 *
 * @author jie.zhang123
 * @since 2019-03-04
 */
public interface ArticlesMapper extends BaseMapper<Articles> {

//    List<Articles> getArticles(Map<String, Object> map, Pagination pagination);
    /**
     * @notes 获得最新五条文章的id
     * @Author junyang.li
     * @Date 11:19 2019/4/1
     **/
    List<Integer> getFiveNewest(@Param("orgId")Integer orgId);

    /**
     * 根据条件查询文章列表
     * @param map
     * @param pagination
     * @return
     */
    List<ArticlesDTO> getArticlesByCondition(Map<String, Object> map, Pagination pagination);
}
