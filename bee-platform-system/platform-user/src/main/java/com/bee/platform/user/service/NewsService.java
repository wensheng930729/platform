 package com.bee.platform.user.service;

 import com.baomidou.mybatisplus.mapper.Wrapper;
 import com.baomidou.mybatisplus.plugins.Page;
 import com.baomidou.mybatisplus.plugins.pagination.Pagination;
 import com.baomidou.mybatisplus.service.IService;
 import com.bee.platform.business.rq.NewsInfoRQ;
 import com.bee.platform.common.entity.ResponseResult;
 import com.bee.platform.user.dto.NewDTO;
 import com.bee.platform.user.entity.News;
 import com.bee.platform.user.entity.NewsType;

 import java.io.Serializable;
 import java.util.List;

/**
 * @author Raphael.dq
 * @date 2019/03/04
 */
public interface NewsService extends IService<News> {

    ResponseResult<Page<News>> listByPage(Pagination pagination, Wrapper<News> wrapper);

    ResponseResult<NewDTO> listByPageable(Pagination page, Wrapper<News> wrapper);

    ResponseResult<Long> countAll();
    
    ResponseResult<News> hitNews(Serializable id);

    ResponseResult addNews(String username, NewsInfoRQ newsInfoRQ);

    ResponseResult updateNews(NewsInfoRQ newsInfoRQ);

    ResponseResult deleteNews(int id);

    ResponseResult delete(List<Integer> ids);

    ResponseResult getInnerOneNews(int id);

    ResponseResult<List<NewsType>> queryNewsTypes();

    ResponseResult addOrUpdateNews(Integer managerId, NewsInfoRQ newsInfoRQ);
}
