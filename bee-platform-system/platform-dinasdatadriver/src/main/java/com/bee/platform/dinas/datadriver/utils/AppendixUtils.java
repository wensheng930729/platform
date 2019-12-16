package com.bee.platform.dinas.datadriver.utils;

import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import com.bee.platform.dinas.datadriver.rq.DinasUrlRQ;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

/**
 * @ClassName AppendixUtils
 * @Description 功能描述
 * @author jie.chen
 * @Date 2019/8/19$ 10:28$
 * @version 1.0.0
 */
@Component
public class AppendixUtils {
    public String getJsonStr(List<DinasUrlRQ> rq){
        JSONArray json = new JSONArray();
        if (!CollectionUtils.isEmpty(rq)){
            for(DinasUrlRQ r : rq){
                JSONObject jo = new JSONObject();
                jo.put("uid", r.getUid());
                jo.put("name", r.getName());
                jo.put("status", r.getStatus());
                jo.put("url", r.getUrl());
                json.add(jo);
            }
        }
        return json.toJSONString();
    }
}
